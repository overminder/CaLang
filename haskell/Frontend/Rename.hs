module Frontend.Rename (
  rename,
) where

-- Resolves names and transforms (Program Name) into (Program Operand).
-- Assigns VReg to each local variables.
-- Will also check undefined symbols.

import Control.Applicative
import Prelude hiding (mapM)
import Control.Monad.State hiding (forM, mapM)
import Control.Monad.Trans
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable

import Frontend.AST
import Backend.Operand
import qualified Backend.Operand as Op
import Utils.Unique

data RenameState
  = MkState {
    rnGlobals :: Map String Operand,
    rnExports :: [String],
    rnClobberedRegs :: [Operand],
    rnCFuncs :: Set String
  }

type CompilerM = UniqueM -- for now
type RenameT = StateT RenameState

evalRenameT :: MonadUnique m => RenameT m a -> m a
evalRenameT m = evalStateT m empty_state
  where
    empty_state = MkState {
      rnGlobals = Map.empty,
      rnExports = [],
      rnClobberedRegs = [],
      rnCFuncs = Set.empty
    }

rename :: MonadUnique m => Program Name ->
          m ([Data Operand], [Func Operand], [String], [Reg])
rename xs = evalRenameT $ do
  -- Two passes
  mapM_ scanToplevel xs
  tops <- liftM join (mapM renameToplevel xs)

  let (w_datas, w_funcs) = List.partition is_data tops
      datas = map unwrap_data w_datas
      funcs = map unwrap_func w_funcs
  exports <- gets rnExports
  clobRegs <- gets (map unReg . rnClobberedRegs)
  return (datas, funcs, exports, clobRegs)
  where
    is_data (DataDef _) = True
    is_data _ = False
    unwrap_data (DataDef d) = d
    unwrap_func (FuncDef f) = f
    unReg (OpReg r) = r

-- Worker functions

newGlobal :: MonadUnique m => StorageType -> String -> RenameT m ()
newGlobal ty name = modify $ \st -> st {
  rnGlobals = Map.insert name (OpImm (NamedLabel name)) (rnGlobals st) 
}

newExport :: MonadUnique m => String -> RenameT m ()
newExport name = modify $ \st -> st {
  rnExports = name:rnExports st
}

newExportC :: MonadUnique m => String -> RenameT m ()
newExportC name = modify $ \st -> st {
  rnExports = name:rnExports st,
  rnCFuncs = Set.insert name (rnCFuncs st)
}

newGlobalReg :: MonadUnique m => String -> Operand -> RenameT m ()
newGlobalReg name reg = modify $ \st -> st {
  rnGlobals = Map.insert name reg (rnGlobals st),
  rnClobberedRegs = reg:rnClobberedRegs st
}

resolveGlobal :: String -> RenameState -> Maybe Operand
resolveGlobal name st = Map.lookup name (rnGlobals st)

-- First pass on toplevel: scan global defs
scanToplevel :: MonadUnique m => ToplevelDef Name -> RenameT m ()
scanToplevel t = case t of
  FuncDef (Func name _ _ _) -> newGlobal u32 name
  DataDef (LiteralData (ty, name) _) -> newGlobal ty name
  ScopeDef scope -> case scope of
    Export names conv -> do
      if conv
        then mapM_ newExportC names
        else mapM_ newExport names
    Import names -> mapM_ (newGlobal i64) names
    GlobalReg name reg -> newGlobalReg name (OpReg reg)

renameToplevel :: MonadUnique m => ToplevelDef Name ->
                  RenameT m [ToplevelDef Operand]
renameToplevel t = case t of
  FuncDef f@(Func _ _ _ _) ->
    liftM (pure . FuncDef) (renameFunc f)
  DataDef _ -> do
    dctLookup <- gets (flip resolveGlobal)
    let f s = case dctLookup s of
                Just v -> v
                Nothing -> error $ "Rename.renameToplevel: " ++
                                   "unknown global var: " ++ s
    return . pure $ fmap f t
  ScopeDef _ ->
    -- Scope defs are stripped since they are no longer needed:
    -- Exports and global regs are accumulated
    -- Imports are just used for type checking
    return []

renameFunc :: MonadUnique m => Func Name -> RenameT m (Func Operand)
renameFunc (Func name args body _) = do
  isC <- gets (Set.member name . rnCFuncs)
  (Just name') <- gets (resolveGlobal name)
  evalFuncT $ do
    -- 1st pass
    args' <- forM args $ \(ty, name) -> do
      op <- newRegV ty
      addLocal name op
      return (ty, op)
    scanStmt body
    -- 2nd pass
    nameResolver <- mkNameResolver
    let body' = fmap nameResolver body
    return $ Func name' args' body' isC

type FuncT m = StateT LocalRenameState (RenameT m)

evalFuncT :: MonadUnique m => FuncT m a -> RenameT m a
evalFuncT = flip evalStateT empty_state
  where
    empty_state = MkLocalState Map.empty

data LocalRenameState
  = MkLocalState {
    localNames :: Map String Operand
  }

addLocal :: MonadUnique m => Name -> Operand -> FuncT m ()
addLocal name op = modify $ \st -> st {
    localNames = let dct = localNames st
                     dup = Map.member name dct
                  in if dup
                       then error $ "Rename.addLocal: duplicate local " ++
                                    "binding: " ++ name
                       else Map.insert name op dct
  }

mkNameResolver :: MonadUnique m => FuncT m (Name -> Operand)
mkNameResolver = do
  localDct <- gets localNames
  globalDct <- lift $ gets rnGlobals
  let f name = case Map.lookup name localDct of
                 Just op -> op
                 Nothing -> do
                   case Map.lookup name globalDct of
                     Just op -> op
                     Nothing -> 
                       error $ "Rename.mkNameResolver: unknown var: " ++
                               name ++ errMsg
                       where
                         localVars = show (Map.keys localDct)
                         globalVars = show (Map.keys globalDct)
                         errMsg = ". Local vars: " ++
                                  localVars ++ ", global vars: " ++ 
                                  globalVars
  return f

-- First pass on function: scan local label defs and var defs.
scanStmt :: MonadUnique m => Stmt Name -> FuncT m ()
scanStmt s = case s of
  SVarDecl bs -> forM_ bs $ \(ty, name) -> do
    op <- newRegV ty
    addLocal name op
  SLabel name -> do
    label <- newTempLabel name
    addLocal name (OpImm label)
  SBlock xs -> do
    mapM_ scanStmt xs
  SIf e s1 s2 -> do
    scanStmt s1
    scanStmt s2
  SWhile e s -> do
    scanStmt s
  _ -> return ()

