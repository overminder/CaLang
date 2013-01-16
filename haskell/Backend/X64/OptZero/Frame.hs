module Backend.X64.OptZero.Frame (
  Frame(..),
  GcMap(..),
  FrameM,
  evalFrameM,
  genPlatDepCode,

  mkBaseIndexAddr, mkBaseAddr,
  argRegs,
) where

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (toList)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Middleend.FlowGraph.Builder as Fg
import Backend.Operand
import Backend.RegAlloc.Liveness
import Backend.X64.Instr
import Utils.Unique

natSize = 8

data Frame
  = Frame {
    flowGraph :: Fg.FlowGraph (Liveness Instr), -- with physical regs
    clobberedRegs :: [Reg], -- global regs
    allocatedRegs :: [Reg], -- used in prolog/epilog
    stackPtr :: Int, -- non-neg, to be sub'd in the prologue
    gcptrOffsets :: Set Int, -- by bytes
    regMap :: Map RegId Int,  -- RegId -> frame ptr offset
    gcMaps :: [GcMap],
    prependInstr :: [Liveness Instr] -> [Liveness Instr],
    epilogRestoreInstrs :: [Liveness Instr]
  }

mStackPtr f = \st -> st { stackPtr = f (stackPtr st) }
modifyStackPtr = modify . mStackPtr
mGcptrOffsets f = \st -> st { gcptrOffsets = f (gcptrOffsets st) }
modifyGcptrOffsets = modify . mGcptrOffsets
mRegMap f = \st -> st { regMap = f (regMap st) }
modifyRegMap = modify . mRegMap
addGcMap m = \st -> st { gcMaps = m:gcMaps st }

data GcMap
  = GcMap {
    gmLabel :: Imm, -- the label for return addr
    gmGcptrOffsets :: [Int], -- relative to rbp
    gmGcptrRegs :: [Reg] -- callee-saved registers that contain gcptrs
  }

type FrameM = StateT Frame UniqueM

evalFrameM :: Fg.FlowGraph (Liveness Instr) -> [Reg] -> Set Reg ->
              FrameM a -> UniqueM a
evalFrameM g clobs allocRegs = (`evalStateT` emptyFrame)
  where
    emptyFrame = Frame {
      flowGraph = g,
      clobberedRegs = clobs,
      allocatedRegs = Set.toList allocRegs,
      stackPtr = 0,
      gcptrOffsets = Set.empty,
      regMap = Map.empty,
      gcMaps = [],
      prependInstr = id,
      epilogRestoreInstrs = []
    }

genPlatDepCode :: FrameM (Fg.FlowGraph Instr)
genPlatDepCode = do
  blocks <- liftM2 getBlockTrace (gets (Fg.blockTrace . flowGraph))
                                 (gets (Fg.blockMap . flowGraph))
  blocks <- forM blocks insertCallSiteRegSave
  blocks <- forM blocks insertPrologAndEpilog
  gets (fmap instr . insertBlocks blocks . flowGraph)
  where
    getBlockTrace ids bmap = map (bmap Map.!) ids
    insertBlocks bs g = foldr Fg.putBlock g bs

insertCallSiteRegSave :: Fg.BasicBlock (Liveness Instr) ->
                         FrameM (Fg.BasicBlock (Liveness Instr))
insertCallSiteRegSave block = do
  toPrepend <- getAndClearPrependInstr
  -- HACK. Note that CALL will insert a mov %rax, xxx, we need to
  -- prepend after that. Indeed a hack.
  toAppend <- case Fg.controlInstr block of
                     Nothing -> return []
                     Just i -> case instr i of
                       CALL _ (needResult, _) -> do
                         let outs = Set.toList (rax `Set.delete` liveOut i)
                         callerSaves <- filterCallerSaves outs
                         (saves, restores) <- liftM unzip (
                           mapM mkCallerSaveRestoreInstr callerSaves)
                         setPrependInstr (\xs -> case xs of
                                            y:ys -> if needResult
                                                     then (y:restores) ++ ys
                                                     else restores ++ (y:ys)
                                            _ -> restores)
                         return saves
                       _ -> return []

  return $ block {
    Fg.instrList = toPrepend (Fg.instrList block) ++ toAppend
  }

filterCallerSaves :: [Reg] -> FrameM [Reg]
filterCallerSaves rs = do
  clobs <- gets clobberedRegs
  return $ filter (isCallerSave clobs) rs
  where
    isCallerSave clobs r = if r `elem` clobs
                             then False
                             else if r `elem` callerSaves
                               then True
                               else False

getAndClearPrependInstr :: FrameM ([Liveness Instr] -> [Liveness Instr])
getAndClearPrependInstr = do
  is <- gets prependInstr
  modify $ \st -> st { prependInstr = id }
  return is

mkCallerSaveRestoreInstr :: Reg -> FrameM (Liveness Instr, Liveness Instr)
mkCallerSaveRestoreInstr r = do
  loc <- stackAlloc r
  return (mkEmptyLiveness (movq (OpReg r) loc),
          mkEmptyLiveness (movq loc (OpReg r)))

setPrependInstr :: ([Liveness Instr] -> [Liveness Instr]) -> FrameM ()
setPrependInstr is = modify $ \st -> st { prependInstr = is }

insertPrologAndEpilog :: Fg.BasicBlock (Liveness Instr) ->
                         FrameM (Fg.BasicBlock (Liveness Instr))
insertPrologAndEpilog block = do
  let instrs = Fg.instrList block ++ (toList (Fg.controlInstr block))
  instrs <- liftM concat (mapM replacePrologWithSaves instrs)
  instrs <- liftM concat (mapM replaceEpilogWithRestores instrs)
  instrs <- liftM concat (mapM adjustStackFrame instrs)
  return $ block { Fg.instrList = init instrs
                 , Fg.controlInstr = Just (last instrs)
                 }

-- XXX: remember to add to GcMap
-- We rely on the invariant that prolog instr can only occur once in
-- a function.
replacePrologWithSaves :: Liveness Instr -> FrameM [Liveness Instr]
replacePrologWithSaves i = case instr i of
  PROLOG -> do
    clobs <- gets clobberedRegs
    aRegs <- gets allocatedRegs
    saves <- execWriterT $ do
      forM aRegs $ \r -> do
        if r `elem` clobs
          then return ()
          else if r `elem` calleeSaves
            then do
              offset <- lift $ stackAllocRaw natSize
              let dest = OpAddr . mkFrameAddr $ (offset - natSize)
                  saveInstr = movq (OpReg r) dest
                  restoreInstr = movq dest (OpReg r)
              tell [mkEmptyLiveness saveInstr]
              modify $ \st -> st {
                epilogRestoreInstrs = epilogRestoreInstrs st ++
                                      [mkEmptyLiveness restoreInstr]
              }
            else return ()
    return (i:saves)
  _ -> return [i]

-- We dont see real EPILOG since RET (True,..)/JMP op (Just ...) represent
-- EPILOG good enough.
replaceEpilogWithRestores :: Liveness Instr -> FrameM [Liveness Instr]
replaceEpilogWithRestores i = case instr i of
  RET _ -> handleEpilog
  JMP _ (Just _) -> handleEpilog
  _ -> return [i]
  where
    handleEpilog = do
      restores <- gets epilogRestoreInstrs
      return $ restores ++ [i]

adjustStackFrame i = case instr i of
  PROLOG -> do
    spOffset <- gets stackPtr
    let instrs = [ PUSH (OpReg rbp)
                 , movq (OpReg rsp) (OpReg rbp)
                 ] ++ spAdj
        spAdj = if spOffset == 0
                  then []
                  else [SUB (OpImm (IntVal (fromIntegral spOffset)))
                            (OpReg rsp)]
    return (fmap mkEmptyLiveness instrs)
  RET _ -> handleEpilog
  JMP _ (Just _) -> handleEpilog
  _ -> return [i]
  where
    handleEpilog = do
      let instrs = [ movq (OpReg rbp) (OpReg rsp)
                   , POP (OpReg rbp)
                   ]
      return $ (fmap mkEmptyLiveness instrs) ++ [i]

-- Allocate (or retrive if allocated previously) a location on the stack
-- and return its address.
stackAlloc :: Reg -> FrameM Operand
stackAlloc r = case r of
  RegP (PhysicalReg (rid, _, _, gcf)) -> do
    mb <- gets (Map.lookup rid . regMap)
    offset <- case mb of
                Just offset -> return offset
                Nothing -> do
                  offset <- stackAllocRaw natSize
                  modifyRegMap (Map.insert rid offset)
                  return offset
    if getGcFlag gcf
      then markGcPtr offset
      else return ()
    return . OpAddr . mkFrameAddr $ (offset - natSize)

stackAllocRaw :: Int -> FrameM Int
stackAllocRaw siz = do
  offset <- gets (negate . stackPtr)
  modifyStackPtr (+siz)
  return offset

markGcPtr :: Int -> FrameM ()
markGcPtr offset = do
  modifyGcptrOffsets (Set.insert offset)

mkFrameAddr i = MkAddr {
  baseOfAddr = Just rbp,
  indexOfAddr = Nothing,
  dispOfAddr = Just (IntVal (fromIntegral i))
}

mkBaseIndexAddr base index = MkAddr {
  baseOfAddr = Just base,
  indexOfAddr = Just (index, 1),
  dispOfAddr = Nothing
}

mkBaseAddr base = MkAddr {
  baseOfAddr = Just base,
  indexOfAddr = Nothing,
  dispOfAddr = Nothing
}

argRegs = map OpReg [rdi, rsi, rdx, rcx, r8, r9]

calleeSaves = [rbx, r12, r13, r14, r15]

callerSaves = generalRegs List.\\ calleeSaves

returnReg = rax

