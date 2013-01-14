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
import Backend.Operand hiding (newVReg)
import qualified Backend.Operand as Op
import qualified Utils.Unique as Unique

type CompilerM = Unique.UniqueM
type MunchM = WriterT [Instr] CompilerM

runMunchM :: MunchM a -> CompilerM [Instr]
runMunchM = execWriterT

munch :: Stmt Operand -> MunchM ()
munch body = do
  emit PROLOG
  munchStmt body

emit = tell . (:[])

newVReg :: StorageType -> MunchM Reg
newVReg ty = do
  OpReg r <- lift $ Op.newVReg ty
  return r

munchStmt s = case s of
  SAssign e1 e2 -> case e1 of
    EVar (OpReg r1) -> do
      o2 <- munchExpr e2
      emit (MOV r1 o2)
    EUnary (MRef (_, w, _)) e1' -> do
      OpReg r1 <- munchExpr e1'
      o2 <- munchExpr e2
      emit (STORE w r1 o2)
    _ -> error $ "Munch.munchStmt: not a valid assign stmt: " ++ show s
  SIf e (SJump (EVar (OpImm lbl_true))) s_false -> do
    lbl_false <- case s_false of
      SJump (EVar op@(OpImm lbl)) -> return lbl
      SBlock [] -> do
        OpImm lbl <- lift (newTempLabel "ifFalse")
        return lbl
      _ -> error $ "X64.Munch: unsupported instr"
    case e of
      EUnary LNot e -> do
        munchStmt (SIf e (SJump (EVar (OpImm lbl_true)))
                         (SJump (EVar (OpImm lbl_false))))
      EBinary bop e1 e2 -> do
        case bop of
          LOr -> do
            lbl_e1_false <- lift (newTempLabel "ifE1False")
            lbl_end <- lift (newTempLabel "ifOrEnd")
            munchStmt (SIf e1 (SJump (EVar (OpImm lbl_true)))
                              (SJump (EVar lbl_e1_false)))
            emit (LABEL (un_imm lbl_e1_false))
            munchStmt (SIf e2 (SJump (EVar (OpImm lbl_true)))
                              (SJump (EVar (OpImm lbl_false))))
            where
              un_imm (OpImm i) = i
          _ -> do
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
    --emit EPILOG
    emit $ RET mbR
  SJump dest -> case dest of
    EVar (OpImm lbl) -> emit (JMP lbl)
  SExpr e -> case e of
    ECall conv func args -> do
      func' <- case func of
                 EVar op@(OpImm (NamedLabel s)) -> return op
                 _ -> munchExpr func
      args' <- mapM munchExpr args
      emit (CALL conv Nothing func' args')
    _ -> munchExpr e >> return ()
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
    OpReg r -> return op
    OpImm i -> do
      tmp <- newVReg i64
      emit (MOV tmp op)
      return (OpReg tmp)
  EBinary bop e1 e2 -> do
    tmp <- newVReg i64
    r1 <- munchExpr e1
    r2 <- munchExpr e2
    emit (BINOP bop tmp r1 r2)
    return (OpReg tmp)
  EUnary uop e -> do
    tmp <- newVReg i64
    r <- munchExpr e
    case uop of 
      MRef (_, w, _) -> 
        emit (LOAD w tmp r)
      _ -> do
        emit (UNROP uop tmp r)
    return (OpReg tmp)
  ECall conv func args -> do
    func' <- case func of
               EVar op@(OpImm (NamedLabel s)) -> return op
               _ -> munchExpr func
    args' <- mapM munchExpr args
    (tmp, res) <- if TailCall `elem` conv || Noret `elem` conv
                    then return (Nothing, error "tailcall has no value")
                    else do
                      tmp <- newVReg i64
                      return (Just tmp, OpReg tmp)
    emit (CALL conv tmp func' args')
    return res

mkInt :: Int -> Operand
mkInt = OpImm . IntVal . fromIntegral

