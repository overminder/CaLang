module Backend.RegAlloc.Liveness (
  iterLiveness,
  Liveness(..),
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint

import qualified Middleend.FlowGraph.Builder as Fg
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

pprRegSet = braces . hsep . punctuate comma . map pprReg . Set.toList

isSameLiveness :: Liveness a -> Liveness a -> Bool
isSameLiveness a b = (liveIn a, liveOut a) == (liveIn b, liveOut b)

iterLiveness :: Instruction a => Fg.FlowGraph a -> Fg.FlowGraph (Liveness a)
iterLiveness rawGraph = go exitBlockIds initialLivenessGraph
  where
    exitBlockIds = filter hasNoSucc (Map.keys (Fg.blockMap rawGraph))
    hasNoSucc = flip Fg.hasNoSucc rawGraph
    initialLivenessGraph = fmap (getDefUse . mkEmptyLiveness) rawGraph

    -- Iteration starts from here
    go :: Instruction a => [Fg.BlockId] ->
          Fg.FlowGraph (Liveness a) -> Fg.FlowGraph (Liveness a)
    go works g = case works of
      [] -> g
      bid:rest -> let b = Fg.getBlock bid g
                      succBlocks = map (flip Fg.getBlock g)
                                       (Set.toList (Fg.getSuccBlockIds bid g))
                      succLiveIn = foldr Set.union Set.empty (
                                     map liveInOfBlock succBlocks)
                      (b', changed) = recalculateBlock b succLiveIn
                   in if changed
                        then go (rest ++ (Set.toList
                                           (Fg.getPredBlockIds bid g)))
                                (Fg.putBlock b' g)
                        else go rest g

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

liveInOfBlock :: Fg.BasicBlock (Liveness a) -> Set Reg
liveInOfBlock = liveIn . head . Fg.instrList

recalculateBlock :: Fg.BasicBlock (Liveness a) ->
                    Set Reg -> (Fg.BasicBlock (Liveness a), Bool)
recalculateBlock b succLiveIns = (b', changed)
  where
    iter (nextIn, changed) thisLv
      = let thisLv' = calculateLiveness nextIn thisLv
            changed' = changed || not (isSameLiveness thisLv thisLv')
         in ((liveIn thisLv', changed'), thisLv')
    ((_, changed), b') = mapAccumR iter (succLiveIns, False) b

-- Transfer function
calculateLiveness :: Set Reg -> Liveness a -> Liveness a
calculateLiveness nextLiveIn this = this'
  where
    this' = this {
      liveOut = nextLiveIn,
      liveIn = uses this `Set.union` (nextLiveIn Set.\\ defs this)
    }

