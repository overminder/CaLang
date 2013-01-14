module Frontend.Rename (
  runRenameM,
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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable

import Frontend.AST
import Backend.Operand hiding (newVReg, newTempLabel)
import qualified Backend.Operand as Op
import Utils.Unique (Unique)
import qualified Utils.Unique as Unique

data RenameState
  = MkState {
    rnGlobals :: Map String Operand,
    rnExports :: [String],
    rnClobberedRegs :: [Operand]
  }

type CompilerM = Unique.UniqueM -- for now
type RenameM = StateT RenameState CompilerM

mkUnique :: RenameM Unique
mkUnique = lift Unique.mkUnique

runRenameM :: RenameM a -> CompilerM a
runRenameM m = evalStateT m empty_state
  where
    empty_state = MkState {
      rnGlobals = Map.empty,
      rnExports = [],
      rnClobberedRegs = []
    }

rename :: Program Name -> RenameM ([Data Operand], [Func Operand],
                                   [String], [Operand])
rename xs = do
  -- Two passes
  mapM_ scanToplevel xs
  tops <- liftM join (mapM renameToplevel xs)

  let (w_datas, w_funcs) = List.partition is_data tops
      datas = map unwrap_data w_datas
      funcs = map unwrap_func w_funcs
  exports <- liftM rnExports get
  clobRegs <- liftM rnClobberedRegs get
  return (datas, funcs, exports, clobRegs)
  where
    is_data (DataDef _) = True
    is_data _ = False
    unwrap_data (DataDef d) = d
    unwrap_func (FuncDef f) = f

-- Worker functions

newGlobal :: StorageType -> String -> RenameM ()
newGlobal ty name = modify $ \st -> st {
  rnGlobals = Map.insert name (OpImm (NamedLabel name)) (rnGlobals st) 
}

newExport :: String -> RenameM ()
newExport name = modify $ \st -> st {
  rnExports = name:rnExports st
}

newGlobalReg :: String -> Operand -> RenameM ()
newGlobalReg name reg = modify $ \st -> st {
  rnGlobals = Map.insert name reg (rnGlobals st),
  rnClobberedRegs = reg:rnClobberedRegs st
}

resolveGlobal :: String -> RenameState -> Maybe Operand
resolveGlobal name st = Map.lookup name (rnGlobals st)

-- First pass on toplevel: scan global defs
scanToplevel :: ToplevelDef Name -> RenameM ()
scanToplevel t = case t of
  FuncDef (Func name _ _) -> newGlobal u32 name
  DataDef (LiteralData (ty, name) _) -> newGlobal ty name
  ScopeDef scope -> case scope of
    Export names conv -> mapM_ newExport names
    Import names -> mapM_ (newGlobal i64) names
    GlobalReg name reg -> newGlobalReg name (OpReg reg)

renameToplevel :: ToplevelDef Name -> RenameM [ToplevelDef Operand]
renameToplevel t = case t of
  FuncDef f@(Func _ _ _) ->
    liftM (pure . FuncDef) (renameFunc f)
  DataDef _ -> do
    dctLookup <- liftM (flip resolveGlobal) get
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

renameFunc :: Func Name -> RenameM (Func Operand)
renameFunc (Func name args body) = do
  (Just name') <- liftM (resolveGlobal name) get
  runFuncM $ do
    -- 1st pass
    args' <- forM args $ \(ty, name) -> do
      op <- lift $ newVReg ty
      addLocal name op
      return (ty, op)
    scanStmt body
    -- 2nd pass
    nameResolver <- mkNameResolver
    let body' = fmap nameResolver body
    return $ Func name' args' body'

type FuncM = StateT LocalRenameState RenameM

runFuncM :: FuncM a -> RenameM a
runFuncM = flip evalStateT empty_state
  where
    empty_state = MkLocalState Map.empty

data LocalRenameState
  = MkLocalState {
    localNames :: Map String Operand
  }

addLocal :: Name -> Operand -> FuncM ()
addLocal name op = modify $ \st -> st {
    localNames = let dct = localNames st
                     dup = Map.member name dct
                  in if dup
                       then error $ "Rename.addLocal: duplicate local " ++
                                    "binding: " ++ name
                       else Map.insert name op dct
  }

mkNameResolver :: FuncM (Name -> Operand)
mkNameResolver = do
  localDct <- liftM localNames get
  globalDct <- lift $ liftM rnGlobals get
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

-- width -> vreg
newVReg ty = lift $ Op.newVReg ty

newTempLabel name = lift $ Op.newTempLabel name

-- First pass on function: scan local label defs and var defs.
scanStmt :: Stmt Name -> FuncM ()
scanStmt s = case s of
  SVarDecl bs -> forM_ bs $ \(ty, name) -> do
    op <- lift $ newVReg ty
    addLocal name op
  SLabel name -> do
    label <- lift $ newTempLabel name
    addLocal name label
  SBlock xs -> do
    mapM_ scanStmt xs
  SIf e s1 s2 -> do
    scanStmt s1
    scanStmt s2
  SWhile e s -> do
    scanStmt s
  _ -> return ()

