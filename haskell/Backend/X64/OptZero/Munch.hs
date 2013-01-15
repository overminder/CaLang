module Backend.X64.OptZero.Munch (
  evalMunchM,
  munchGraph,
) where

import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe (isJust, maybeToList)
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Middleend.Tac.Instr as Tac
import qualified Middleend.FlowGraph.Builder as Fg
import Backend.Operand
import Backend.X64.OptZero.Frame
import Backend.X64.Instr
import Utils.Unique

data MunchState
  = MunchState {
    instrForNextBlock :: Maybe Instr
  }

type MunchM = State MunchState

evalMunchM :: MunchM a -> a
evalMunchM = (`evalState` emptyState)
  where
    emptyState = MunchState {
      instrForNextBlock = Nothing
    }

modifyInstrForNextBlock f = modify $ \st -> st {
  instrForNextBlock = f (instrForNextBlock st)
}

munchGraph :: Fg.FlowGraph Tac.Instr -> MunchM (Fg.FlowGraph Instr)
munchGraph g@(Fg.MkGraph name args isC entry blocks pm sm lm) = do
  -- Here we build a trace of the flow since Call may need to
  -- emit mov %rax, %dest to the next block.
  let blockTrace = map (blocks Map.!) (Fg.mkTrace g)
  munchedBlocks <- forM blockTrace $ \block -> do
    instrs <- liftM2 (++) getAndClearPrevInstr
                          (execWriterT (munchBlock block))
    return $ Fg.MkBlock {
      Fg.blockId = Fg.blockId block,
      Fg.instrList = init instrs,
      Fg.controlInstr = Just (last instrs),
      Fg.blockLabels = Fg.blockLabels block
    }
  let newBlockMap = Map.fromList (map mkBlockPair munchedBlocks)
  return $ Fg.MkGraph name args isC entry newBlockMap pm sm lm
  where
    mkBlockPair b = (Fg.blockId b, b)
    getAndClearPrevInstr = do
      prevInstr <- gets (maybeToList . instrForNextBlock)
      modifyInstrForNextBlock (const Nothing)
      return prevInstr

munchBlock :: Fg.BasicBlock Tac.Instr -> WriterT [Instr] MunchM ()
munchBlock (Fg.MkBlock bid instrs (Just ctrlInstr) _) = do
  --emit (LABEL (BlockLabel bid))
  mapM_ munchInstr instrs
  munchInstr ctrlInstr

-- Note the syntax switch (Intel -> AT&T)
munchInstr :: Tac.Instr -> WriterT [Instr] MunchM ()
munchInstr tac = case tac of
  Tac.LABEL i -> error $ "munchInstr: labels should not appear"

  Tac.PROLOG -> emit PROLOG 

  Tac.EPILOG -> emit EPILOG

  Tac.MOV dest src -> do
    emit (MOV src (OpReg dest))

  Tac.BINOP op dest (OpReg lhs) (OpReg rhs) -> case op of
    AAdd -> emit (LEA (OpAddr (mkBaseIndexAddr lhs rhs)) (OpReg dest))
    ASub -> do
      emit (MOV (OpReg lhs) (OpReg dest))
      emit (SUB (OpReg rhs) (OpReg dest))
    AMul -> undefined
    ADiv -> undefined

  Tac.UNROP op dest (OpReg src) -> case op of
    _ -> undefined

  Tac.LOAD width dest (OpReg src) -> do
    let op1 = OpAddr (mkBaseAddr src)
        op2 = OpReg dest
    case width of
      W64 -> emit (MOV op1 op2)
      _ -> emit (MOVSxQ width op1 op2)

  Tac.STORE width dest (OpReg src) -> do
    let op1 = OpReg src
        op2 = OpAddr (mkBaseAddr dest)
    case width of
      W64 -> emit (MOV op1 op2)
      _ -> emit (MOVSxQ width op1 op2)

  Tac.CASEJUMP _ _ -> undefined

  Tac.RET mb -> case mb of
    Nothing -> emit (RET False)
    Just (OpReg r) -> do
      emit (MOV (OpReg r) (OpReg rax))
      emit (RET True)

  Tac.JIF cond (lhs, rhs) trueLabel _ -> do
    emit (CMP rhs lhs)
    emit (JXX cond trueLabel)

  Tac.JMP op -> do
    emit (JMP (OpImm op) Nothing)

  Tac.CALL conv mbRes func args -> do
    let instrCtor = case TailCall `elem` conv of
                      True -> \op info -> JMP op (Just info)
                      False -> CALL
        mkArg = case CCall `elem` conv of
                      True -> case Vararg `elem` conv of
                        True -> mkCVararg
                        False -> mkCArg
                      False -> mkCaLangArg
        callInstr = instrCtor func
    (argLocs, preCallInstrs) <- mkArg args
    forM (zip args argLocs) $ \(src, dest) -> do
      emit (MOV src dest)
    tell preCallInstrs
    emit (callInstr (isJust mbRes, argLocs))
    case mbRes of
      Just dest -> addToNextBlock (MOV (OpReg rax) (OpReg dest))
      Nothing -> return ()

addToNextBlock instr = modifyInstrForNextBlock $ \mbi -> case mbi of
  Nothing -> Just instr
  Just _ -> error $ "addToNextBlock: there's already an instr!"

-- XXX: not using stack at all

assertArgLength args = if length args > 6 then error "too many args" else args
mkCArg args = do
  return $ (zipWith const argOps (assertArgLength args), [])
mkCVararg args = do
  (locs, _) <- mkCArg args
  return (locs, [XOR (OpReg eax) (OpReg eax)])
  
-- For now
mkCaLangArg = mkCArg

emit = tell . (:[])
