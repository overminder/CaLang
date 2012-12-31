module Backend.X64.Munch (
  munch,
  runMunchM,
) where

-- Direct translation from AST to X64 instruction

import Control.Monad.Writer

import Frontend.AST
import Backend.X64.Instr
import Backend.X64.Operand
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
      emit (MOV r2 r1)
    EUnary (MRef ty) e1' -> do
      r1 <- munchExpr e1'
      r2 <- munchExpr e2
      emit (MOV (mkAddrB r1) r2)
    _ -> error $ "Munch.munchStmt: not a valid assign stmt: " ++ show s
  SIf e (SJump (EVar (OpImm lbl_true))) s_false -> do
    case e of
      EUnary LNot (EBinary bop e1 e2) -> do
        r1 <- munchExpr e1
        r2 <- munchExpr e2
        emit (CMP r1 r2)
        emit (JXX (reverseCond bop) lbl_true)
      EUnary LNot e -> do
        r <- munchExpr e
        emit (CMP r (OpImm (IntVal 0)))
        emit (JXX REq lbl_true)
      EBinary bop e1 e2 -> do
        r1 <- munchExpr e1
        r2 <- munchExpr e2
        emit (CMP r1 r2)
        emit (JXX bop lbl_true)
      _ -> do
        r <- munchExpr e
        emit (CMP r (mkInt 0))
        emit (JXX REq lbl_true)
    case s_false of
      SJump (EVar op@(OpImm lbl_false)) -> do
        emit (JMP op)
      SBlock [] -> return ()
      _ -> error $ "X64.Munch: unsupported instr"
  SBlock xs -> mapM_ munchStmt xs
  SReturn mbE -> do
    case mbE of
      Just e -> do
        r <- munchExpr e
        emit (MOV r (OpReg (PReg returnReg)))
      Nothing -> return ()
    emit RET
  SJump dest -> case dest of
    EVar lbl@(OpImm _) -> emit (JMP lbl)
    _ -> do
      r <- munchExpr dest
      emit (JMP r)
  SExpr e -> munchExpr e >> return ()
  SLabel lbl@(OpImm _) -> emit (LABEL lbl)
  SSwitch e lbls (Just tab) -> do
    r <- munchExpr (EUnary (MRef i64)
                           (EBinary AAdd
                                    (EVar tab)
                                    (EBinary BShl
                                             e
                                             (EVar (OpImm (IntVal 3))))))
    emit (SWITCH r (map (\(OpImm i) -> i) lbls))
  _ -> error $ "Munch.munchStmt: " ++ show s

munchExpr :: Expr Operand -> MunchM Operand
munchExpr e = case e of
  EVar op -> case op of
    OpReg r -> return op
    OpImm i -> do
      -- XXX: use rip-indirection mode for shared lib
      tmp <- lift $ newVReg i64
      emit (MOV op tmp)
      return tmp
  EBinary bop e1 e2 -> do
    tmp <- lift $ newVReg i64
    r1 <- munchExpr e1
    emit (MOV r1 tmp)
    case bop of
      BShl -> case e2 of
        EVar i2@(OpImm (IntVal shift_by)) -> emit (SHL i2 tmp)
        _ -> error $ "munchExpr: non-const shift not supported yet: " ++ show e
      _ -> do
        r2 <- munchExpr e2
        emit (arithOpToInstr bop r2 tmp)
    return tmp
  EUnary uop e -> case uop of 
    MRef ty -> do
      r <- munchExpr e
      tmp <- lift $ newVReg i64
      let instr = chooseMovFromType ty
      emit (instr (mkAddrB r) tmp)
      return tmp
    _ -> do
      r <- munchExpr e
      tmp <- lift $ newVReg i64
      emit (MOV r tmp)
      emit (uopToInstr uop tmp)
      return tmp
  ECall conv func args -> do
    func' <- case func of
               -- XXX: add @PLT for shared lib
               EVar op@(OpImm (NamedLabel s)) -> return op
               _ -> munchExpr func
    args' <- mapM munchExpr args
    forM (zip argRegs args') $ \(pr, vr) -> do
      emit (MOV vr (OpReg (PReg pr)))
    if Vararg `elem` conv
      then emit (XOR (OpReg (PReg rax)) (OpReg (PReg rax)))
      else return ()
    if TailCall `elem` conv
      then emit (JMP func') >> return (OpReg (PReg returnReg))
      else emit (CALL func') >> return (OpReg (PReg returnReg))

chooseMovFromType :: StorageType -> Operand -> Operand -> Instr
chooseMovFromType (_, w, _) = case w of
  W64 -> MOV
  _ -> MOVSxQ w

mkAddrB :: Operand -> Operand
mkAddrB (OpReg r) = OpAddr (MkAddr (Just r) Nothing Nothing)

mkAddrBI :: Operand -> Operand -> Int -> Operand
mkAddrBI (OpReg rb) (OpReg ri) i
  = OpAddr (MkAddr (Just rb) (Just (ri, i)) Nothing)

mkInt :: Int -> Operand
mkInt = OpImm . IntVal . fromIntegral

arithOpToInstr :: MachOp -> Operand -> Operand -> Instr
arithOpToInstr op = case op of
  AAdd -> ADD
  ASub -> SUB
  AMul -> MUL
  ADiv -> DIV
  _ -> error $ "Munch.arithOpToInstr: " ++ show op

uopToInstr :: MachOp -> Operand -> Instr
uopToInstr op = case op of
  ANeg -> NEG
  BNot -> NOT

