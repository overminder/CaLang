{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Middleend.Tac.LocalOpt (
  runOpt,
) where

import Control.Monad.State hiding (forM_)
import Data.Foldable
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (foldr)
import Text.PrettyPrint

import Middleend.Tac.Instr
import Middleend.FlowGraph.Builder
import Backend.Operand hiding (newVReg)
import qualified Backend.Operand as Op
import Utils.Class
import qualified Utils.Unique as Unique

-- Block-level local optimization

-- for instr in instr_list:
--     if instr.is_arith:
--         vreg = new_vreg
--         set_alias(instr.dest, vreg)
--         instr.set_dest(vreg)
--         instr.replace_operand_with_alias
--         instr.memorize # a1 := b1 + c2, memorize a1 as (b1 + c2)
--     ...
--
-- for instr in instr_list:
--     if instr.is_arith:
--         if instr.rhs.is_avaiable:
--             instr.replace_operand_with_alias
-- 
-- for instr in instr_list:
--     if instr.is_pure and use(instr.lhs) == None:
--         instr.remove

data Expr
  = Unary MachOp Operand
  | Binary MachOp Operand Operand
  deriving (Eq, Ord)

pprExpr e = case e of
  Unary uop op -> text (showMachOp uop) <> ppr op
  Binary bop o1 o2 -> ppr o1 <+> text (showMachOp bop) <+> ppr o2

data OptState
  = MkOptState {
    origRegs :: Set Reg,
    needsMoarIter :: Bool, -- Mutable var, used by passes to indicate whether
                           -- should run again or not.
    hasNoSucc :: Bool,
    thisBlock :: BasicBlock Instr
  }

type CompilerM = Unique.UniqueM
type OptM = StateT OptState CompilerM

runOpt g = do
  blocks' <- forM blocks $ \(bid, b) -> do
    b' <- run_block b
    return (bid, (fmap strip_info b'))
  return $ MkGraph {
    entryBlock = entryBlock g,
    blockMap = Map.fromList blocks',
    predMap = predMap g,
    succMap = succMap g,
    labelMap = labelMap g
  }
  where
    blocks = Map.toList (blockMap g)
    run_block b = evalStateT (run_passes b) (scanBlock g b)
    strip_info = fst
    run_passes :: BasicBlock Instr -> OptM (BasicBlock InstrWithInfo)
    run_passes b = do
      let is = instrList b
      ifs <- runInstrM doValueNumbering (zip is (repeat (emptyInfo, emptyInfo)))
      ifs <- runInstrM gatherAvailExpr ifs
      ifs <- insertPhiFunction ifs
      ifs <- runIterInstrM (runInstrM all_opts) ifs
      ifs <- runIterInstrM doDCE ifs
      return $ MkBlock {
        blockId = blockId b,
        instrList = ifs,
        controlInstr = fmap (\i -> (i, (emptyInfo, emptyInfo)))
                            (controlInstr b),
        blockLabels = blockLabels b
      }
    all_opts i = do
      i <- doAliasProp i
      i <- doCSE i
      return i

newVReg :: StorageType -> OptM Operand
newVReg ty = lift $ Op.newVReg ty

scanBlock :: FlowGraph Instr -> BasicBlock Instr -> OptState
scanBlock g b = MkOptState {
  origRegs = foldr scan_instr Set.empty (instrList b),
  needsMoarIter = False,
  hasNoSucc = has_nothing (Map.lookup (blockId b) (succMap g)),
  thisBlock = b
}
  where
    scan_instr instr rs = foldr Set.insert rs (getDefOfInstr instr)
    has_nothing m = case m of
      Nothing -> True
      Just some -> Set.null some

-- Info for in and out
type InstrInfoPair = (InstrInfo, InstrInfo)
type TransferFunc = InstrInfo -> InstrInfo
type InstrM = StateT (InstrInfoPair, TransferFunc) OptM
type InstrWithInfo = (Instr, InstrInfoPair)

data InstrInfo
  = MkInstrInfo {
    currNumbering :: Map Reg Reg, -- orig -> current alias
    regAliasMap :: Map Reg Reg, -- alias -> orig
    availExpr :: Map Expr Reg, -- expr -> current ae
    constTemp :: Map Reg Imm,  -- reg  -> current aimm
    liveness :: Set Reg -- current liveness
  }

pprInstrInfo (MkInstrInfo _ aliases ae ct lv)
  = text "AE:" <+> comma_join (map ppr_ae (Map.toList ae)) <+>
    text "Const:" <+> comma_join (map ppr_ct (Map.toList ct)) <+>
    --text "Alias:" <+> comma_join (map ppr_alias (Map.toList aliases))
    text "Liveness:" <+> comma_join (map pprReg (Set.toList lv))
  where
    comma_join xs = brackets (hcat (punctuate comma xs))
    ppr_ae (exp, r) = pprExpr exp <+> text "==" <+> pprReg r
    ppr_ct (r, imm) = pprReg r <+> text "==" <+> pprImm imm
    ppr_alias (wat, points_to)
      = pprReg wat <+> text "points to" <+> pprReg points_to

instance Ppr InstrWithInfo where
  ppr (i, (info_in, info_out)) =
    ppr i <+> text ";; IN:" <+> pprInstrInfo info_in <+>
              text "out:" <+> pprInstrInfo info_out

flip3 f b c a = f a b c

emptyInfo = MkInstrInfo {
  currNumbering = Map.empty,
  regAliasMap = Map.empty,
  availExpr = Map.empty,
  constTemp = Map.empty,
  liveness = Set.empty
}

-- Each change of an InstrInfoPair will be passed to both
-- the in and the out successing instrinfos
runInstrM :: (Instr -> InstrM Instr) -> [InstrWithInfo] ->
             OptM [InstrWithInfo]
runInstrM m instr_w_infos = do
  (iff, _) <- flip3 foldM ([], id) instr_w_infos $ iter_once
  return iff
  where
    iter_once (result, tfunc) (instr, (my_in, my_out)) = do
      let my_in' = tfunc my_in
      let my_out' = tfunc my_out
      (instr, (info, my_tfunc)) <- runStateT (m instr) ((my_in', my_out'), id)
      return (result ++ [(instr, info)], my_tfunc . tfunc)

setNeedsMoarIter b = modify $ \st -> st {
  needsMoarIter = b
}

runIterInstrM m ifs = do
  setNeedsMoarIter False
  ifs' <- m ifs
  needs_moar <- liftM needsMoarIter get
  if needs_moar
    then runIterInstrM m ifs'
    else return ifs'

-- Number the reg-val of lhs
doValueNumbering :: Instr -> InstrM Instr
doValueNumbering instr = case instr of
  MOV r1 o2 -> do
    o2' <- findCurrAlias o2
    r1' <- assignNewNumber r1
    return $ MOV r1' o2'
  BINOP bop r1 o2 o3 -> do
    o2' <- findCurrAlias o2
    o3' <- findCurrAlias o3
    r1' <- assignNewNumber r1
    return $ BINOP bop r1' o2' o3'
  UNROP uop r1 o2 -> do
    o2' <- findCurrAlias o2
    r1' <- assignNewNumber r1
    return $ UNROP uop r1' o2'
  LOAD w r1 o2 -> do
    o2' <- findCurrAlias o2
    r1' <- assignNewNumber r1
    return $ LOAD w r1' o2'
  STORE w r1 o2 -> do
    o2' <- findCurrAlias o2
    OpReg r1' <- findCurrAlias (OpReg r1)
    return $ LOAD w r1' o2'
  _ -> return instr -- pro/epilog
--

applyTFunc :: (InstrInfo -> InstrInfo) -> InstrM ()
applyTFunc tfunc = modify $ \((info_in, info_out), old_tfunc) ->
  ((info_in, tfunc info_out), tfunc . old_tfunc)

assignNewNumber :: Reg -> InstrM Reg
assignNewNumber orig = do
  OpReg alias <- lift $ newVReg (getClassOfReg orig)
  let tfunc info_out = info_out {
    --regAliasMap = Map.insert alias orig (regAliasMap info_out),
    currNumbering = Map.insert orig alias (currNumbering info_out)
  }
  applyTFunc tfunc
  return alias

findCurrAlias :: Operand -> InstrM Operand
findCurrAlias o = case o of
  OpReg r -> do
    r' <- liftM (Map.findWithDefault r r . currNumbering . fst . fst) get
    return $ OpReg r'
  OpImm i -> do
    return o

setAliasFor :: Reg -> Reg -> InstrM ()
setAliasFor ptr dest = do
  let tfunc info_out = info_out {
    regAliasMap = Map.insert ptr dest (regAliasMap info_out)
  }
  applyTFunc tfunc

derefAlias :: Reg -> InstrM (Maybe Reg)
derefAlias ptr = liftM (Map.lookup ptr . regAliasMap . fst . fst) get

removeAliasPair :: Reg -> Reg -> InstrM ()
removeAliasPair ptr dest = do
  let tfunc info_out = info_out {
    regAliasMap = Map.delete ptr (regAliasMap info_out)
  }
  applyTFunc tfunc

addAliasPair :: Reg -> Reg -> InstrM ()
addAliasPair ptr dest = do
  let tfunc info_out = info_out {
    regAliasMap = Map.insert ptr dest (regAliasMap info_out)
  }
  applyTFunc tfunc

-- Record the avail-expr of rhs
gatherAvailExpr :: Instr -> InstrM Instr
gatherAvailExpr instr = do
  case instr of
    MOV r1 o2 ->
      case o2 of
        OpReg r2 -> addAliasPair r1 r2
        OpImm i -> addConstTemp r1 i
    BINOP bop r1 o2 o3 -> do
      addAvailExpr (Binary bop o2 o3) r1
    UNROP uop r1 o2 -> do
      addAvailExpr (Unary uop o2) r1
    _ -> pass
  return instr

addAvailExpr :: Expr -> Reg -> InstrM ()
addAvailExpr e r = do
  let tfunc info_out = info_out {
    availExpr = Map.insertWith (flip const) e r (availExpr info_out)
  }
  applyTFunc tfunc

addConstTemp :: Reg -> Imm -> InstrM ()
addConstTemp r i = applyTFunc tfunc
  where
    tfunc info_out = info_out {
      constTemp = Map.insert r i (constTemp info_out)
    }

removeConstTempFor :: Reg -> InstrM ()
removeConstTempFor r = applyTFunc tfunc
  where
    tfunc info_out = info_out {
      constTemp = Map.delete r (constTemp info_out)
    }

insertPhiFunction :: [InstrWithInfo] -> OptM [InstrWithInfo]
insertPhiFunction ifs = do
  let (_, (_, last_out)) = last ifs
      aliases = currNumbering last_out
      mk_phi r = case Map.lookup r aliases of
                   Nothing -> id
                   Just r' -> (:) (MOV r (OpReg r'))
  phis <- liftM (Set.foldr mk_phi [] . origRegs) get
  phifs <- runInstrM gatherAvailExpr (zip phis (repeat (last_out, last_out)))
  return $ ifs ++ phifs

-- add %dest, %o2, %o3 where (+ %o2 %o3) is an available expr defined by
-- %ae ==> mov %dest, %ae; set %dest to be an alias for %ae.
doCSE :: Instr -> InstrM Instr
doCSE instr = case instr of
  BINOP bop dest o2 o3 -> do
    maybe_ae <- findAvailExpr (Binary bop o2 o3)
    case maybe_ae of
      Just ae -> do
        lift $ setNeedsMoarIter True
        setAliasFor dest ae
        return $ MOV dest (OpReg ae)
      Nothing -> return instr
  _ -> return instr

-- mov %dest, %src where %src is an alias of %alias
-- ==> mov %dest, %alias, remove alias pair (%dest, %src) and add alias pair
--     (%dest, %alias)
doAliasProp :: Instr -> InstrM Instr
doAliasProp i = case i of
  MOV dest o2 -> do
    case o2 of
      OpReg src -> do
        maybe_alias <- derefAlias src
        case maybe_alias of
          Just alias -> do
            lift $ setNeedsMoarIter True
            removeAliasPair dest src
            addAliasPair dest alias
            return $ MOV dest (OpReg alias)
          Nothing -> do
            return i
      _ -> return i
  BINOP bop dest o2 o3 -> do
    o2' <- case o2 of
      OpReg src -> do
        maybe_alias <- derefAlias src
        case maybe_alias of
          Just alias -> do
            lift $ setNeedsMoarIter True
            return $ (OpReg alias)
          Nothing -> do
            return o2
    o3' <- case o3 of
      OpReg src -> do
        maybe_alias <- derefAlias src
        case maybe_alias of
          Just alias -> do
            lift $ setNeedsMoarIter True
            return $ (OpReg alias)
          Nothing -> do
            return o3
    return $ BINOP bop dest o2' o3'
  _ -> return i

findAvailExpr :: Expr -> InstrM (Maybe Reg)
findAvailExpr e = liftM (Map.lookup e . availExpr . fst . fst) get

findConstTemp :: Reg -> InstrM (Maybe Imm)
findConstTemp r = liftM (Map.lookup r . constTemp . fst . fst) get

-- Reversed analysis
-- Mainly used in d/u analysis
runRevInstrM :: (Instr -> InstrM Instr) -> [InstrWithInfo] ->
                OptM [InstrWithInfo]
runRevInstrM m instr_w_infos = do
  (iff, _) <- flip3 foldrM ([], id) instr_w_infos $ iter_once
  return iff
  where
    iter_once (instr, (my_in, my_out)) (result, tfunc) = do
      let my_in' = tfunc my_in
      let my_out' = tfunc my_out
      (instr, (info, my_tfunc)) <- runStateT (m instr) ((my_in', my_out'), id)
      return ((instr, info):result, my_tfunc . tfunc)

scanLiveness :: Instr -> InstrM Instr
scanLiveness instr = do
  applyTFuncRev $ \info_in -> info_in {
    liveness = modify_liveness (liveness info_in)
  }
  return instr
  where
    defs = getDefOfInstr instr
    uses = getUseOfInstr instr
    modify_liveness liveness = flip execState liveness $ do
      forM_ defs $ \adef -> do
        modify $ Set.delete adef
      forM_ uses $ \ause -> do
        modify $ Set.insert ause

applyTFuncRev :: (InstrInfo -> InstrInfo) -> InstrM ()
applyTFuncRev tfunc = modify $ \((info_in, info_out), old_tfunc) ->
  ((tfunc info_in, info_out), tfunc . old_tfunc)

setInitLiveness :: [InstrWithInfo] -> OptM [InstrWithInfo]
setInitLiveness instrs = do
  has_no_succ <- liftM hasNoSucc get
  lvset <- if has_no_succ
    then do
      uses <- liftM (getUseOfInstr . fromJust . controlInstr . thisBlock) get
      return $ Set.fromList uses
    else do
      liftM origRegs get
  return $ map (add_liveness lvset) instrs
  where
    add_liveness lvset (i, (info_in, info_out)) = (i, (info_in {
      liveness = lvset
    }, info_out {
      liveness = lvset
    }))


doDCE :: [InstrWithInfo] -> OptM [InstrWithInfo]
doDCE ifs = do
  ifs <- setInitLiveness ifs
  ifs <- runRevInstrM scanLiveness ifs
  let ifs' = map remove_dead_code ifs
  return (catMaybes ifs')
  where
    remove_dead_code :: InstrWithInfo -> Maybe InstrWithInfo
    remove_dead_code iwi@(instr, (_, info_out)) = case getDefOfInstr instr of
      [] -> Just iwi
      [r] -> if Set.member r (liveness info_out)
        then Just iwi
        else Nothing

