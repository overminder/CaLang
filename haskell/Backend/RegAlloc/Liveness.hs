module Backend.RegAlloc.Liveness (
  iterLiveness,
  iterDCE,
  Liveness(..),
  mkEmptyLiveness,
) where

import Prelude hiding (foldr)
import Control.Monad.Identity
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable
import Data.Traversable
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint

import qualified Middleend.FlowGraph.Builder as Fg
import qualified Middleend.FlowGraph.Analysis as Fg
import Backend.Operand
import Utils.Class

data Liveness a = Liveness {
  instr :: a,
  defs :: Set Reg,
  uses :: Set Reg,
  liveIn :: Set Reg,
  liveOut :: Set Reg
}

instance Ppr a => Ppr (Liveness a) where
  ppr (Liveness i ds us ins outs)
    = ppr i <+> text "defs=" <> pprRegSet ds <+> text "uses=" <>
      pprRegSet us <+> text "liveIn=" <> pprRegSet ins <+> text "liveOut=" <>
      pprRegSet outs

instance Instruction a => Instruction (Liveness a) where
  isBranchInstr = isBranchInstr . instr
  localBranchTargets = localBranchTargets . instr
  isLabelInstr = isLabelInstr . instr
  getLabelOfInstr = getLabelOfInstr . instr
  isFallThroughInstr = isFallThroughInstr . instr
  getFallThroughTarget = getFallThroughTarget . instr
  mkJumpInstr _ = undefined
  renameBranchInstrLabel _ = undefined
  getUseOfInstr _ = undefined
  getDefOfInstr _ = undefined
  replaceRegInInstr f (Liveness i ds us lin lout)
    = Liveness (replaceRegInInstr f i) (Set.map f ds) (Set.map f us)
               (Set.map f lin) (Set.map f lout)
  isPureInstr = isPureInstr . instr

pprRegSet = braces . hsep . punctuate comma . map pprReg . Set.toList

isSameLiveness :: Liveness a -> Liveness a -> Bool
isSameLiveness a b = (liveIn a, liveOut a) == (liveIn b, liveOut b)

iterLiveness :: Instruction a => Fg.FlowGraph a -> Fg.FlowGraph (Liveness a)
iterLiveness rawGraph = fst . runIdentity $
  Fg.iterAnalysis (Fg.Bwd calculateLiveness) initialLivenessGraph
  where
    initialLivenessGraph = fmap (getDefUse . mkEmptyLiveness) rawGraph

-- Transfer function
calculateLiveness :: (Monad m, Instruction a) =>
                     Liveness a -> [Liveness a] -> m (Liveness a, Bool)
calculateLiveness this succs = return (this', changed)
  where
    nextLiveIn = foldr Set.union Set.empty (map liveIn succs)
    this' = this {
      liveOut = nextLiveIn,
      liveIn = uses this `Set.union` (nextLiveIn Set.\\ defs this)
    }
    changed = not (this' `isSameLiveness` this)

mkEmptyLiveness :: a -> Liveness a
mkEmptyLiveness i = Liveness {
  instr = i,
  defs = Set.empty,
  uses = Set.empty,
  liveIn = Set.empty,
  liveOut = Set.empty
}

getDefUse :: Instruction a => Liveness a -> Liveness a
getDefUse liveness@(Liveness {instr=i}) = liveness {
  defs = Set.fromList (getDefOfInstr i),
  uses = Set.fromList (getUseOfInstr i)
}

-- Remove dead code so that regalloc can work fine
iterDCE :: Instruction a =>
           Fg.FlowGraph (Liveness a) -> Fg.FlowGraph (Liveness a)
iterDCE g =
  let (g', changed) = runIdentity (Fg.iterAnalysis (Fg.Alter eliminateIfDead) g)
   in if changed
        then let (g'', _) = runIdentity (Fg.iterAnalysis
                                          (Fg.Bwd calculateLiveness) g')
              in iterDCE g''
        else g

eliminateIfDead :: (Monad m, Instruction a) =>
                   Liveness a -> m ([Liveness a], Bool)
eliminateIfDead i = case Set.toList (defs i) of
  [] -> return ([i], False)
  [d] -> do
    if Set.notMember d (liveOut i) && isPureInstr (instr i)
      then return ([], True)
      else return ([i], False)

