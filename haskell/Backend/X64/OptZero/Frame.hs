module Backend.X64.OptZero.Frame (
  Frame(..),
  GcMap(..),
  FrameM,
  evalFrameM,
  genPlatDepCode,

  mkBaseIndexAddr, mkBaseAddr,
  kArgRegs, kCallerSaves, kCalleeSaves,
) where

import Control.Arrow hiding ((<+>))
import Control.Monad.State
import Control.Monad.Writer hiding ((<>))
import Data.Foldable (toList)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint

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
    calleeSaveInfo :: [(Reg, Operand)], -- callee-save regs and their locations

    -- GC-related:
    -- We maintain those information in the GcMap:
    -- * Offsets of saved callee-saved regs
    -- * Offsets of saved local gcptr
    -- * Escaped (through callee-saved regs) local gcptr
    gcptrOffsets :: Set Int, -- by bytes
    regMap :: Map RegId Int,  -- RegId -> frame ptr offset.
    gcMaps :: [GcMap],
    prevGcMap :: Maybe GcMap,

    -- Temp values passed during code gen
    prependInstr :: [Liveness Instr] -> [Liveness Instr]
  }

mStackPtr f = \st -> st { stackPtr = f (stackPtr st) }
modifyStackPtr = modify . mStackPtr
mGcptrOffsets f = \st -> st { gcptrOffsets = f (gcptrOffsets st) }
modifyGcptrOffsets = modify . mGcptrOffsets
mRegMap f = \st -> st { regMap = f (regMap st) }
modifyRegMap = modify . mRegMap
addGcMap m = \st -> st { gcMaps = m:gcMaps st }

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
      calleeSaveInfo = error "calleeSaveInfo",

      gcptrOffsets = Set.empty,
      regMap = Map.empty,
      gcMaps = [],
      prevGcMap = Nothing,

      prependInstr = id
    }

-- Insert reg save/restore at callsite and prolog/epilog.
-- Also deal with Gc interface (make gcmap, insert labels, etc).
-- We currently always generate gcmap (as it is zero cost to the runtime).
genPlatDepCode :: FrameM (Fg.FlowGraph Instr, [GcMap])
genPlatDepCode = do
  mkSpaceForCalleeSaveRegs
  blocks <- liftM2 getBlockTrace (gets (Fg.blockTrace . flowGraph))
                                 (gets (Fg.blockMap . flowGraph))
  blocks <- forM blocks insertCallSiteRegSave
  blocks <- forM blocks insertPrologAndEpilog
  finalGraph <- gets (fmap instr . insertBlocks blocks . flowGraph)
  rawGcMap <- gets gcMaps
  spOffset <- gets stackPtr
  let patchedGcMap = map (\gcm -> gcm { gmFramePtrOffset = spOffset + natSize })
                         rawGcMap
  return (finalGraph, patchedGcMap)
  where
    getBlockTrace ids bmap = map (bmap Map.!) ids
    insertBlocks bs g = foldr Fg.putBlock g bs

-- For each callsite, insert caller-reg-save instructions before the call
-- and restore instruction after the call.
--
-- Also, when GC interface is required, this will make a new gcmap.
insertCallSiteRegSave :: Fg.BasicBlock (Liveness Instr) ->
                         FrameM (Fg.BasicBlock (Liveness Instr))
insertCallSiteRegSave block = do
  toPrepend <- getAndClearPrependInstr
  attachLabelToPrevGcMap (Fg.blockId block)
  toAppend <- mkAppendInstr

  return $ block {
    Fg.instrList = toPrepend (Fg.instrList block) ++ toAppend
  }
  where
    mkAppendInstr = case Fg.controlInstr block of
      Nothing -> return []
      Just i -> case instr i of
        CALL _ (needResult, _) -> do
          -- CALL normally don't define any reg except rax when the call
          -- result is needed. Therefore, we need to consider rax as
          -- not live across the call when needResult == True.
          let rawOuts = liveOut i
              cleanedOuts = Set.toList $ if needResult
                                           then Set.delete rax rawOuts
                                           else rawOuts
          callerSaves <- filterCallerSaves cleanedOuts
          escapedRegs <- filterEscapedRegs cleanedOuts
          locToSave <- mapM stackAlloc callerSaves
          let saveInstr = zipWith mkSaveInstr callerSaves locToSave
              restoreInstr = zipWith mkRestoreInstr callerSaves locToSave
              prepender xs = case xs of
                -- HACK. Note that CALL will insert a mov %rax, xxx,
                -- we need to prepend restore instr after that.
                y:ys -> if needResult
                          then (y:restoreInstr) ++ ys
                          else restoreInstr ++ (y:ys)
                _ -> restoreInstr
          prepareNewGcMap (zip callerSaves locToSave) escapedRegs
          setPrependInstr prepender
          return saveInstr
        _ -> return []
    mkSaveInstr r loc = mkEmptyLiveness (movq (OpReg r) loc)
    mkRestoreInstr r loc = mkEmptyLiveness (movq loc (OpReg r))

-- Make a new gcmap with caller-save information for this callsite.
prepareNewGcMap :: [(Reg, Operand)] -> [Reg] -> FrameM ()
prepareNewGcMap savedAtCallSite escapes = do
  savedRegs <- gets (map (second unwrapFrameLoc) . calleeSaveInfo)
  let gcOffsets = map (unwrapFrameLoc . snd)
                      (filter (regIsGc . fst) savedAtCallSite)
      gcEscapes = filter regIsGc escapes

  let newGcMap = GcMap {
    gmLabel = error "empty label",
    gmPrologSavedRegs = savedRegs,
    gmEscapedRegs = gcEscapes,
    gmFramePtrOffset = error "fpOffset not defined",
    gmGcptrOffsets = gcOffsets
  }
  modify $ \st -> st { prevGcMap = Just newGcMap }
  where
    unwrapFrameLoc (OpAddr (MkAddr _ _ (Just (IntVal offset))))
      = fromIntegral offset
    regIsGc = getGcFlag . gcFlagOfReg

attachLabelToPrevGcMap :: Fg.BlockId -> FrameM ()
attachLabelToPrevGcMap bid = do
  mbGcMap <- gets prevGcMap
  case mbGcMap of
    Nothing -> return ()
    Just gcMap -> do
      modify $ \st -> st {
        gcMaps = gcMap { gmLabel = BlockLabel bid } : (gcMaps st),
        prevGcMap = Nothing
      }

filterCallerSaves :: [Reg] -> FrameM [Reg]
filterCallerSaves rs = do
  clobs <- gets clobberedRegs
  return $ filter (isCallerSave clobs) rs
  where
    isCallerSave clobs r = if r `elem` clobs
                             then False
                             else if r `elem` kCallerSaves
                               then True
                               else False

filterEscapedRegs :: [Reg] -> FrameM [Reg]
filterEscapedRegs rs = do
  clobs <- gets clobberedRegs
  return $ filter (isEscape clobs) rs
  where
    isEscape clobs r = if r `elem` clobs
                         then False
                         else if r `elem` kCalleeSaves
                           then True
                           else False

getAndClearPrependInstr :: FrameM ([Liveness Instr] -> [Liveness Instr])
getAndClearPrependInstr = do
  is <- gets prependInstr
  modify $ \st -> st { prependInstr = id }
  return is

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

findCalleeSaveRegUses :: FrameM [Reg]
findCalleeSaveRegUses = do
  allUses <- gets allocatedRegs
  return $ filter (`elem` kCalleeSaves) allUses

mkSpaceForCalleeSaveRegs :: FrameM ()
mkSpaceForCalleeSaveRegs = do
  regsToSave <- findCalleeSaveRegUses
  regLocPairs <- forM regsToSave $ \r -> do
    offset <- stackAllocRaw natSize
    let dest = OpAddr . mkFrameAddr $ (offset - natSize)
    return (r, dest)
  modify $ \st -> st { calleeSaveInfo = regLocPairs }

-- XXX: remember to add to GcMap
-- We rely on the invariant that prolog instr can only occur once in
-- a function.
replacePrologWithSaves :: Liveness Instr -> FrameM [Liveness Instr]
replacePrologWithSaves i = case instr i of
  PROLOG -> do
    regLocPairs <- gets calleeSaveInfo
    let saveInstrs = map mkSaveInstr regLocPairs
    return (i:saveInstrs)
  _ -> return [i]
  where
    mkSaveInstr (r, loc) = mkEmptyLiveness (movq (OpReg r) loc)

-- We dont see real EPILOG since RET (True,..)/JMP op (Just ...) represent
-- EPILOG good enough.
replaceEpilogWithRestores :: Liveness Instr -> FrameM [Liveness Instr]
replaceEpilogWithRestores i = case instr i of
  RET _ -> handleEpilog
  JMP _ (Just _) -> handleEpilog
  _ -> return [i]
  where
    handleEpilog = do
      regLocPairs <- gets calleeSaveInfo
      let restoreInstrs = map mkRestoreInstr regLocPairs
      return $ restoreInstrs ++ [i]
    mkRestoreInstr (r, loc) = mkEmptyLiveness (movq loc (OpReg r))

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

kArgRegs = [rdi, rsi, rdx, rcx, r8, r9]

kCalleeSaves = [rbx, r12, r13, r14, r15]

kCallerSaves = generalRegs List.\\ kCalleeSaves

kReturnReg = rax
