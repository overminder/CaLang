module Middleend.Tac.Munch (
  munch,
  runMunchM,
) where

-- Direct translation from AST to Tac instruction

import Prelude hiding (mapM)
import Control.Monad.Writer hiding (mapM)
import Data.Traversable

import Frontend.AST
import Middleend.Tac.Instr
import Backend.Operand
import qualified Utils.Unique as Unique

type CompilerM = Unique.UniqueM
type MunchM = WriterT [Instr] CompilerM

runMunchM :: MunchM a -> CompilerM [Instr]
runMunchM = execWriterT

munch :: Func Operand -> MunchM ()
munch (Func name args body) = munchStmt body

emit = tell . (:[])

munchStmt s = case s of
  SAssign e1 e2 -> case e1 of
    EVar r1@(OpReg _) -> do
      r2 <- munchExpr e2
      emit (MOV r1 r2)
    EUnary (MRef (_, w, _)) e1' -> do
      r1 <- munchExpr e1'
      r2 <- munchExpr e2
      emit (STORE w r1 r2)
    _ -> error $ "Munch.munchStmt: not a valid assign stmt: " ++ show s
  SIf e (SJump (EVar (OpImm lbl_true))) s_false -> do
    lbl_false <- case s_false of
      SJump (EVar op@(OpImm lbl)) -> return lbl
      SBlock [] -> do
        OpImm lbl <- lift (newTempLabel "ifFalse")
        return lbl
      _ -> error $ "X64.Munch: unsupported instr"
    case e of
      EUnary LNot (EBinary bop e1 e2) -> do
        r1 <- munchExpr e1
        r2 <- munchExpr e2
        emit (JIF (reverseCond bop) (r1, r2) lbl_true lbl_false)
      EUnary LNot e -> do
        r <- munchExpr e
        emit (JIF RNe (r, mkInt 0) lbl_true lbl_false)
      EBinary bop e1 e2 -> do
        r1 <- munchExpr e1
        r2 <- munchExpr e2
        emit (JIF bop (r1, r2) lbl_true lbl_false)
      _ -> do
        r <- munchExpr e
        emit (JIF REq (r, mkInt 0) lbl_true lbl_false)
    emit (LABEL lbl_false)
  SBlock xs -> mapM_ munchStmt xs
  SReturn mbE -> do
    mbR <- mapM munchExpr mbE
    emit $ RET mbR
  SJump dest -> case dest of
    EVar (OpImm lbl) -> emit (JMP lbl)
  SExpr e -> munchExpr e >> return ()
  SLabel (OpImm lbl) -> emit (LABEL lbl)
  SSwitch e lbls (Just tab) -> do
    r <- munchExpr (EUnary (MRef i64)
                           (EBinary AAdd
                                    (EVar tab)
                                    (EBinary BShl
                                             e
                                             (EVar (mkInt 3)))))
    emit (CASEJUMP r (map (\(OpImm i) -> i) lbls))
  _ -> error $ "Munch.munchStmt: " ++ show s

munchExpr :: Expr Operand -> MunchM Operand
munchExpr e = case e of
  EVar op -> case op of
    OpReg _ -> return op
    OpImm _ -> do
      tmp <- lift $ newVReg i64
      emit (MOV tmp op)
      return tmp
  EBinary bop e1 e2 -> do
    tmp <- lift $ newVReg i64
    r1 <- munchExpr e1
    r2 <- munchExpr e2
    emit (BINOP bop tmp r1 r2)
    return tmp
  EUnary uop e -> do
    tmp <- lift $ newVReg i64
    r <- munchExpr e
    case uop of 
      MRef (_, w, _) -> 
        emit (LOAD w tmp r)
      _ -> do
        emit (UNROP uop tmp r)
    return tmp
  ECall conv func args -> do
    tmp <- lift $ newVReg i64
    func' <- case func of
               EVar op@(OpImm (NamedLabel s)) -> return op
               _ -> munchExpr func
    args' <- mapM munchExpr args
    emit (CALL conv tmp func' args')
    return tmp

mkInt :: Int -> Operand
mkInt = OpImm . IntVal . fromIntegral

