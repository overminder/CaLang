module Frontend.Simplify (
  runSimplifyM,
  simplify
) where
-- This is a pass after renaming. It does the following things:
--  * Extract string literals and replace them with labels
--  * Lift logical expressions out and turn them into If statements
--  * Lift nested call so that each child of a call cannot be a call
--  * Remove type declaration statements in functions

import Control.Monad.State
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Utils.Unique (Unique)
import qualified Utils.Unique as Unique

import Frontend.AST
import Backend.Operand
import Backend.Class

type CompilerM = Unique.UniqueM
type SimplifyM = StateT SimState CompilerM

mkUnique :: SimplifyM Unique.Unique
mkUnique = lift Unique.mkUnique

data SimState
  = MkState {
    extractedString :: Map.Map String Operand
  }

runSimplifyM m = evalStateT m empty_state
  where
    empty_state = MkState {
      extractedString = Map.empty
    }

simplify :: [Func Operand] -> SimplifyM ([Data Operand], [Func Operand])
simplify funcs = do
  funcs' <- mapM simplifyFunc funcs
  datas <- liftM (mk_datas . extractedString) get
  return (datas, funcs')
  where
    mk_datas m = Map.foldrWithKey combine [] m
    combine str op = (:) (LiteralData (i64, op) (LStr str))

simplifyFunc :: Func Operand -> SimplifyM (Func Operand)
simplifyFunc (Func name args body) = do
  body_ext <- traverseExprM extractStr body
  body_no_lop <- liftLogicalOp body_ext
  body_non_call <- liftNestedCall body_no_lop
  let body_cleaned = cleanDeclStmt body_non_call
  return $ Func name args body_cleaned

extractStr :: Expr Operand -> SimplifyM (Expr Operand)
extractStr e = case e of
  ELit (LStr s) -> do
    label <- intern_str s
    return $ EVar label
  EBinary bop e1 e2 -> do
    e1' <- extract_str e1
    e2' <- extract_str e2
    return $ EBinary bop e1' e2'
  EUnary uop e -> do
    liftM (EUnary uop) (extract_str e)
  ECall conv func args -> do
    func' <- extract_str func
    args' <- mapM extract_str args
    return $ ECall conv func' args'
  _ -> return e
  where
    extract_str = extractStr
    intern_str s = do
      strs <- liftM extractedString get
      case Map.lookup s strs of
        Nothing -> do
          i <- mkUnique
          let new_label = OpImm (TempLabel "str" i)
          modify $ \st -> st {
            extractedString = Map.insert s new_label strs
          }
          return new_label
        Just old_label -> return old_label

-- lift || && ! to if
liftLogicalOp :: Stmt Operand -> SimplifyM (Stmt Operand)
liftLogicalOp s = liftExprM lift_expr s
  where
    lift_expr e = case e of
      EBinary bop e1 e2 -> do
        (e1', s1s) <- lift_expr e1
        (e2', s2s) <- lift_expr e2
        case bop of 
          LAnd -> do
            tmp <- liftM EVar (newVReg i64)
            let check_e1 = SIf e1' check_e2 (mk_set_stmt tmp 0)
                check_e2 = SIf e2' (mk_set_stmt tmp 0) (mk_set_stmt tmp 1)
            return (tmp, s1s ++ s2s ++ [check_e1])
          LOr -> do
            tmp <- liftM EVar (newVReg i64)
            let check_e1 = SIf e1' (mk_set_stmt tmp 1) check_e2
                check_e2 = SIf e2' (mk_set_stmt tmp 1) (mk_set_stmt tmp 0)
            return (tmp, s1s ++ s2s ++ [check_e1])
          _ -> do
            return (EBinary bop e1' e2', s1s ++ s2s)
      EUnary uop e -> do
        (e', ss) <- lift_expr e
        case uop of 
          LNot -> do
            tmp <- liftM EVar (newVReg i64)
            let check_e = SIf e' (mk_set_stmt tmp 0) (mk_set_stmt tmp 1)
            return (tmp, ss ++ [check_e])
          _ -> do
            return (EUnary uop e', ss)
      ECall conv func args -> do
        (func', sfs) <- lift_expr func
        (args', sas) <- liftM unzip (mapM lift_expr args)
        return (ECall conv func' args', sfs ++ concat sas)
      _ -> return (e, [])
    mk_set_stmt op ival = SAssign op (ELit (LInt ival))

liftNestedCall :: Stmt Operand -> SimplifyM (Stmt Operand)
liftNestedCall s = liftExprM lift_expr s
  where
    -- If the parent is not a call
    lift_expr e = case e of
      ECall conv func args -> do
        (func', sfs) <- lift_call func
        (args', sas) <- liftM unzip (mapM lift_call args)
        return (ECall conv func' args', sfs ++ concat sas)
      EBinary uop e1 e2 -> do
        (e1', s1) <- lift_expr e1
        (e2', s2) <- lift_expr e2
        return $ (EBinary uop e1' e2', [])
      EUnary uop e -> do
        (e', s) <- lift_expr e
        return $ (EUnary uop e', [])
      _ -> return (e, [])

    -- If the parent is a call
    lift_call e = case e of
      ECall conv func args -> do
        (func', sfs) <- lift_call func
        (args', sas) <- liftM unzip (mapM lift_call args)
        tmp <- liftM EVar (newVReg i64)
        return (tmp, sfs ++ concat sas ++
                     [SAssign tmp (ECall conv func' args')])
      _ -> lift_expr e

-- clean VarDecl
cleanDeclStmt :: Stmt Operand -> Stmt Operand
cleanDeclStmt stmt = case flatten_clean stmt of
  [one] -> one
  result@_ -> SBlock result
  where
    clean = cleanDeclStmt
    flatten_clean s = case s of
      SVarDecl _ -> []
      SIf e s1 s2 -> [SIf e (clean s1) (clean s2)]
      SWhile e s -> [SWhile e (clean s)]
      SBlock xs -> [SBlock (concatMap flatten_clean xs)]
      _ -> [s]

-- width -> vreg
-- Consider put this in one file?
newVReg :: StorageType -> SimplifyM Operand
newVReg (kls, width, gc) = do
  i <- mkUnique
  return . OpReg $ VReg (MkRegId i) kls width gc


