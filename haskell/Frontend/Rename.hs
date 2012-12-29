module Frontend.Rename (
  runRenameM,
  rename,
) where

import Data.Traversable
import Prelude hiding (mapM)
import Control.Monad.State hiding (forM, mapM)
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import Frontend.AST
import Frontend.Parser

import Backend.Operand
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

runRenameM :: RenameM a -> CompilerM (a, [String], [Operand])
runRenameM m = do
  (a, st) <- runStateT m empty_state
  return (a, rnExports st, rnClobberedRegs st)
  where
    empty_state = MkState {
      rnGlobals = Map.empty,
      rnExports = [],
      rnClobberedRegs = []
    }

rename :: Program Name -> RenameM (Program Operand)
rename xs = do
  mapM_ scanToplevel xs
  liftM catMaybes (mapM renameToplevel xs)

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

-- first pass: scan global defs
scanToplevel :: ToplevelDef Name -> RenameM ()
scanToplevel t = case t of
  FuncDef (Func name _ _) -> newGlobal u32 name
  DataDef (LiteralData (ty, name) _) -> newGlobal ty name
  ScopeDef scope -> case scope of
    Export names -> mapM_ newExport names
    Import names -> mapM_ (newGlobal i64) names
    GlobalReg name reg -> newGlobalReg name (OpReg reg)

renameToplevel :: ToplevelDef Name -> RenameM (Maybe (ToplevelDef Operand))
renameToplevel t = case t of
  FuncDef f@(Func _ _ _) ->
    liftM (Just . FuncDef) (renameFunc f)
  DataDef _ -> do
    dctLookup <- liftM (flip resolveGlobal) get
    let f s = case dctLookup s of
                Just v -> v
                Nothing -> error $ "Rename.renameToplevel: " ++
                                   "unknown global var: " ++ s
    return . Just $ fmap f t
  ScopeDef _ ->
    -- Scope defs are stripped since they are no longer needed:
    -- Exports and global regs are accumulated
    -- Imports are just used for link checking
    return Nothing

renameFunc :: Func Name -> RenameM (Func Operand)
renameFunc (Func name args body) = do
  (Just name') <- liftM (resolveGlobal name) get
  runFuncM $ do
    args' <- forM args $ \(ty, name) -> do
      op <- newVReg ty
      addLocal name op
      return (ty, op)
    scanStmt body
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
                         localDctRepr = show localDct
                         globalDctRepr = show globalDct
                         errMsg = ". Dict traversed: " ++
                                  localDctRepr ++ " and " ++ 
                                  globalDctRepr
  return f


-- width -> vreg
newVReg :: StorageType -> FuncM Operand
newVReg (kls, width, gc) = do
  i <- lift mkUnique
  return . OpReg $ VReg (MkRegId i) kls width gc

newTmpLabel :: String -> FuncM Operand
newTmpLabel s = do
  i <- lift mkUnique
  return . OpImm . TempLabel s $ i

-- first pass: scan local label defs and var defs.
scanStmt :: Stmt Name -> FuncM ()
scanStmt s = case s of
  SVarDecl (ty, name) -> do
    op <- newVReg ty
    addLocal name op
  SLabel name -> do
    label <- newTmpLabel name
    addLocal name label
  SBlock xs -> do
    mapM_ scanStmt xs
  _ -> return ()
