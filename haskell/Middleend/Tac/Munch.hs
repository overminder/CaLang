module Middleend.Tac.Munch (
  munch,
) where

-- Direct translation from AST to Tac instruction

import Prelude hiding (mapM)
import Control.Monad.RWS hiding (mapM)
import Data.Traversable

import Frontend.AST
import Middleend.Tac.Instr
import Backend.Operand
import Utils.Unique

type MunchT m a = RWST WhileStack [Instr] () m a

type WhileStack = [(Imm, Imm)] -- (goto loop, goto end)

evalMunchM :: MonadUnique m => MunchT m a -> m [Instr]
evalMunchM m = liftM snd (execRWST m [] ())

munch :: MonadUnique m => Stmt Operand -> m [Instr]
munch body = evalMunchM $ do
  emit PROLOG
  munchStmt body

emit :: MonadUnique m => Instr -> MunchT m ()
emit = tell . (:[])

munchStmt :: MonadUnique m => Stmt Operand -> MunchT m ()
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
  SIf e s_true s_false -> do
    case e of
      EUnary LNot innerE -> do
        munchStmt (SIf innerE s_false s_true)
      EBinary bop e1 e2 -> do
        case bop of
          LOr -> do
            lbl_do_true <- newTempLabel "ifDoTrue"
            lbl_e1_false <- newTempLabel "ifE1False"
            lbl_do_false <- newTempLabel "ifDoFalse"
            lbl_end <- newTempLabel "ifOrEnd"
            munchStmt (SIf e1 (SJump (EVar (OpImm lbl_do_true)))
                              (SJump (EVar (OpImm lbl_e1_false))))
            emit (LABEL lbl_e1_false)
            munchStmt (SIf e2 (SJump (EVar (OpImm lbl_do_true)))
                              (SJump (EVar (OpImm lbl_do_false))))
            emit (LABEL lbl_do_true)
            munchStmt s_true
            emit (JMP lbl_end)
            emit (LABEL lbl_do_false)
            munchStmt s_false
            emit (LABEL lbl_end)
          LAnd -> do
            lbl_do_true <- newTempLabel "ifDoTrue"
            lbl_e1_true <- newTempLabel "ifE1True"
            lbl_do_false <- newTempLabel "ifDoFalse"
            lbl_end <- newTempLabel "ifOrEnd"
            munchStmt (SIf e1 (SJump (EVar (OpImm lbl_e1_true)))
                              (SJump (EVar (OpImm lbl_do_false))))
            emit (LABEL lbl_e1_true)
            munchStmt (SIf e2 (SJump (EVar (OpImm lbl_do_true)))
                              (SJump (EVar (OpImm lbl_do_false))))
            emit (LABEL lbl_do_true)
            munchStmt s_true
            emit (JMP lbl_end)
            emit (LABEL lbl_do_false)
            munchStmt s_false
            emit (LABEL lbl_end)
          RNe -> munchRel bop e1 e2 s_true s_false
          REq -> munchRel bop e1 e2 s_true s_false
          RLt -> munchRel bop e1 e2 s_true s_false
          RLe -> munchRel bop e1 e2 s_true s_false
          RGt -> munchRel bop e1 e2 s_true s_false
          RGe -> munchRel bop e1 e2 s_true s_false
          BAnd -> munchRel bop e1 e2 s_true s_false
          _ -> munchStmt (SIf (EBinary RNe e (EVar (mkInt 0))) s_true s_false)
      _ -> munchStmt (SIf (EBinary RNe e (EVar (mkInt 0))) s_true s_false)
  SWhile e s -> 
    withWhileBlock $ \lbl_loop lbl_end -> do
      munchStmt $ SBlock ([SIf e (SJump (EVar (OpImm lbl_loop)))
                                 (SJump (EVar (OpImm lbl_end))),
                           SLabel (OpImm lbl_loop)] ++ [s] ++
                          [SIf e (SJump (EVar (OpImm lbl_loop)))
                                 (SJump (EVar (OpImm lbl_end))),
                           SLabel (OpImm lbl_end)])
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

  SContinue -> do
    (lbl_loop, _) <- getLastWhile
    emit (JMP lbl_loop)

  SBreak -> do
    (_, lbl_end) <- getLastWhile
    emit (JMP lbl_end)

  _ -> error $ "Munch.munchStmt: " ++ show s
  where
    withWhileBlock m = do 
      lbl_loop <- newTempLabel "whileLoop"
      lbl_end <- newTempLabel "whileEnd"
      local ((lbl_loop, lbl_end) :) $ do
        m lbl_loop lbl_end
    getLastWhile = do
      stk <- ask
      case stk of
        [] -> error "Tac.munch: continue/break outside while stack"
        x:_ -> return x

munchRel :: MonadUnique m => MachOp -> Expr Operand -> Expr Operand -> 
            Stmt Operand -> Stmt Operand -> MunchT m ()
munchRel op e1 e2 s_true s_false = do
  lbl_true <- newTempLabel "ifTrue"
  lbl_false <- newTempLabel "ifFalse"
  lbl_end <- newTempLabel "ifEnd"
  r1 <- munchExpr e1
  r2 <- munchExpr e2
  emit (JIF op (r1, r2) lbl_true lbl_false)
  emit (LABEL lbl_true)
  munchStmt s_true
  emit (JMP lbl_end)
  emit (LABEL lbl_false)
  munchStmt s_false
  emit (LABEL lbl_end)

munchExpr :: MonadUnique m => Expr Operand -> MunchT m Operand
munchExpr e = case e of
  EVar op -> case op of
    OpReg r -> return op
    OpImm i -> do
      tmp <- newRegV i64
      emit (MOV (unReg tmp) op)
      return tmp
  EAsm r -> return (OpReg r)
  EBinary bop e1 e2 -> do
    tmp <- newRegV i64
    if isLogicOp bop || isCondOp bop
      then do
        fallBackToIf tmp
      else do
        r1 <- munchExpr e1
        r2 <- munchExpr e2
        emit (BINOP bop (unReg tmp) r1 r2)
    return tmp
  EUnary uop e -> do
    tmp <- newRegV i64
    if isLogicOp uop
      then do
        fallBackToIf tmp
      else do
        r <- munchExpr e
        case uop of
          MRef (_, w, _) -> 
            emit (LOAD w (unReg tmp) r)
          _ -> do
            emit (UNROP uop (unReg tmp) r)
    return tmp
  ECall conv func args -> do
    func' <- case func of
               EVar op@(OpImm (NamedLabel s)) -> return op
               _ -> munchExpr func
    args' <- mapM munchExpr args
    (tmp, res) <- if TailCall `elem` conv || Noret `elem` conv
                    then return (Nothing, error "tailcall has no value")
                    else do
                      tmp <- newRegV i64
                      return (Just (unReg tmp), tmp)
    emit (CALL conv tmp func' args')
    return res
  _ -> error $ "Tac.munchExpr: " ++ show e
  where
    fallBackToIf tmp = do
      munchStmt (SIf e (SAssign (EVar tmp) (EVar (mkInt 1)))
                       (SAssign (EVar tmp) (EVar (mkInt 0))))

mkInt :: Int -> Operand
mkInt = OpImm . IntVal . fromIntegral

