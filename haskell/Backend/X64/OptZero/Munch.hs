module Backend.X64.OptZero.Munch where

import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Middleend.Tac.Instr as Tac
import qualified Middleend.FlowGraph.Builder as Fg
import Backend.Operand
import Backend.X64.OptZero.Frame
import Backend.X64.Operand
import Backend.X64.Instr
import Utils.Unique

type MunchM = WriterT [Instr] FrameM

runMunchM :: MunchM a -> FrameM [Instr]
runMunchM m = execWriterT m

munchGraph :: Fg.FlowGraph Tac.Instr -> MunchM ()
munchGraph (Fg.MkGraph name args entry blocks _ _ _) = do
  let entryBlock = blocks Map.! entry
      otherBlocks = Map.delete entry blocks
  munchBlock entryBlock
  mapM_ munchBlock (Map.elems otherBlocks)

munchBlock :: Fg.BasicBlock Tac.Instr -> MunchM ()
munchBlock (Fg.MkBlock bid instrs (Just ctrlInstr) _) = do
  emit (LABEL (BlockLabel bid))
  mapM_ munchInstr instrs
  munchInstr ctrlInstr

-- Note the syntax switch (Intel -> AT&T)
munchInstr :: Tac.Instr -> MunchM ()
munchInstr tac = case tac of
  Tac.LABEL i -> emit (LABEL i)

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
    emit (MOVSxQ width op1 op2)

  Tac.STORE width dest (OpReg src) -> do
    let op1 = OpReg src
        op2 = OpAddr (mkBaseAddr dest)
    emit (MOVSxQ width op1 op2)

  Tac.CASEJUMP _ _ -> undefined

  Tac.RET mb -> case mb of
    Nothing -> emit (RET False)
    Just (OpReg r) -> do
      emit (MOV (OpReg r) (OpReg (PReg rax)))
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
                        True -> mkCArg
                        False -> mkCVararg
                      False -> mkCaArg
    undefined

mkCArg = undefined
mkCVararg = undefined
mkCaArg = undefined

emit = tell . (:[])

