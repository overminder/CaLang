module Frontend.Simplify (
  runSimplifyM,
  simplify
) where
-- Module Simplify is a pass after Rename. It does the following things:
--  * Remove type declaration statements in function
--  * Extract string literals and replace them with labels

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
  body' <- traverseExprM extract_str body
  return $ Func name args body'
  where
    extract_str e = case e of
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

