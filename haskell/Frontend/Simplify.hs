module Frontend.Simplify (
  runSimplifyM,
  simplify,
) where
-- This is a pass after renaming. It does the following things:
--  * Extract string literals in functions and daats and replace them
--    with labels
--  * Lift logical expressions out and turn them into If statements
--  * Lift nested call so that each child of a call cannot be a call
--  * Make and attach jump table for switch statement
--  * Remove literal nodes in functions
--  * Desugar if and while so that functions only contain simple branches:
--    if (e) { goto L1; }
--  * Remove type declaration statements in functions

import Control.Monad.State
import Control.Monad.Trans
import Data.Char (ord)
import Data.Map (Map)
import qualified Data.Map as Map
import Utils.Unique (Unique)
import qualified Utils.Unique as Unique

import Frontend.AST
import Backend.Operand hiding (newVReg, newTempLabel)
import qualified Backend.Operand as Op

type CompilerM = Unique.UniqueM
type SimplifyM = StateT SimState CompilerM

mkUnique :: SimplifyM Unique.Unique
mkUnique = lift Unique.mkUnique

data SimState
  = MkState {
    extractedString :: Map.Map String Operand,
    jumpTable :: [Data Operand]
  }

runSimplifyM m = evalStateT m empty_state
  where
    empty_state = MkState {
      extractedString = Map.empty,
      jumpTable = []
    }

simplify :: [Data Operand] -> [Func Operand] ->
            SimplifyM ([Data Operand], [Func Operand])
simplify datas funcs = do
  funcs' <- mapM simplifyFunc funcs
  datas' <- mapM simplifyData datas
  more_str <- liftM (mk_datas . extractedString) get
  jmp_tab <- liftM jumpTable get
  return (datas' ++ more_str ++ jmp_tab, funcs')
  where
    mk_datas m = Map.foldrWithKey comb_str [] m
    comb_str str op = (:) (LiteralData (i64, op) (LStr str))

runPipeline ms init = case ms of
  x:xs -> do
    init' <- x init
    runPipeline xs init'
  [] -> return init

-- All of the simplifications
simplifyFunc :: Func Operand -> SimplifyM (Func Operand)
simplifyFunc (Func name args body) = do
  body' <- runPipeline [extractStr,
                        extractJumpTable,
                        liftNestedCall,
                        desugarControlStmt,
                        return . cleanLiteral,
                        return . cleanDeclStmt]
                       body
  return $ Func name args body'

simplifyData :: Data Operand -> SimplifyM (Data Operand)
simplifyData (LiteralData (ty, name) lit) = do
  lit' <- case lit of
    LArr xs -> liftM LArr (mapM extract_str xs)
    _ -> return lit
  return (LiteralData (ty, name) lit')
  where
    extract_str lit = case lit of
      LStr s -> liftM LSym (internString s)
      _ -> return lit

extractStr :: Stmt Operand -> SimplifyM (Stmt Operand)
extractStr = traverseExprM extract_str'
  where
    extract_str' e = case e of
      ELit (LStr s) -> do
        label <- internString s
        return $ EVar label
      EBinary bop e1 e2 -> do
        e1' <- extract_str' e1
        e2' <- extract_str' e2
        return $ EBinary bop e1' e2'
      EUnary uop e -> do
        liftM (EUnary uop) (extract_str' e)
      ECall conv func args -> do
        func' <- extract_str' func
        args' <- mapM extract_str' args
        return $ ECall conv func' args'
      _ -> return e

internString :: String -> SimplifyM Operand
internString s = do
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

extractJumpTable :: Stmt Operand -> SimplifyM (Stmt Operand)
extractJumpTable s = case s of
  SSwitch e labels Nothing -> do
    table_name <- mk_jump_table labels
    return $ SSwitch e labels (Just table_name)
  SIf e s1 s2 -> do
    s1' <- transf s1
    s2' <- transf s2
    return $ SIf e s1' s2'
  SWhile e s -> do
    s' <- transf s
    return $ SWhile e s'
  SBlock xs -> liftM SBlock (mapM transf xs)
  _ -> return s
  where
    transf = extractJumpTable
    add_jump_table table = modify $ \st -> st {
      jumpTable = table:jumpTable st
    }
    mk_jump_table labels = do
      i <- mkUnique
      let name = OpImm (TempLabel "jumpTable" i)
          table = LiteralData (i64, name) (LArr (map LSym labels))
      add_jump_table table
      return name

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

-- Such that each basic block is started by a label (exact for entry)
-- and ended by a control instr (ret/jmp/call)
-- ... Well, seems that we cannot do that here (User can insert label
--     anywhere). Instead we ensure this invariant in the flow graph builder.
desugarControlStmt :: Stmt Operand -> SimplifyM (Stmt Operand)
desugarControlStmt s = do
  xs <- desugar s
  return $ if length xs == 1 then head xs else SBlock xs
  where
    flatten s = case s of
      SBlock xs -> xs
      _ -> [s]
    desugar s = case s of
      SBlock xs -> liftM concat (mapM desugar xs)
      SIf e s1 s2 -> do
        s1' <- desugar s1
        s2' <- desugar s2
        lbl_false <- newTempLabel "ifFalse"
        lbl_end <- newTempLabel "ifEnd"
        let ifStmt = SIf (EUnary LNot e)
                         (SJump (EVar lbl_false))
                         (SBlock [])
        return $ [ifStmt] ++ s1' ++
                 [SJump (EVar lbl_end), SLabel lbl_false] ++ s2' ++
                 [SLabel lbl_end]
      SWhile e s -> do
        s' <- desugar s
        lbl_loop <- newTempLabel "whileLoop"
        lbl_check <- newTempLabel "whileCheck"
        lbl_end <- newTempLabel "whileEnd"
        return $ [SIf e (SJump (EVar lbl_loop)) (SJump (EVar lbl_end)),
                  SLabel lbl_loop] ++ s' ++
                 [SLabel lbl_check,
                  SIf e (SJump (EVar lbl_loop)) (SBlock []),
                  SLabel lbl_end]
      _ -> return [s]

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

-- clean ELit
cleanLiteral :: Stmt Operand -> Stmt Operand
cleanLiteral = traverseExpr clean_lit
  where
    clean_lit :: Expr Operand -> Expr Operand
    clean_lit exp = case exp of
      ELit lit -> case lit of
        -- Consider move this to AST.hs?
        LInt i -> mk_int i
        LChr c -> mk_int . fromIntegral . ord $ c
        LFlo d -> mk_flo d
        LSym a -> EVar a
        _ -> error $ "Simplify.cleanLiteral: uncleaned literal: " ++ show lit
      EBinary bop e1 e2 -> let e1' = clean_lit e1
                               e2' = clean_lit e2
                            in EBinary bop e1' e2'
      EUnary uop e -> let e' = clean_lit e
                       in EUnary uop e'
      ECall conv func args -> let func' = clean_lit func
                                  args' = map clean_lit args
                               in ECall conv func' args'
      _ -> exp
      where
        mk_int = EVar . OpImm . IntVal
        mk_flo = EVar . OpImm . FloatVal

newVReg ty = lift $ Op.newVReg ty

newTempLabel name = lift $ Op.newTempLabel name

