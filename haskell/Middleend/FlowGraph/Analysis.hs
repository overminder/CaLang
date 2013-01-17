module Middleend.FlowGraph.Analysis (
  iterBwd,
) where

import Prelude hiding (mapM)
import Data.Foldable
import Data.Traversable
import Control.Monad.State hiding (forM, forM_, mapM, mapM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Middleend.FlowGraph.Builder

-- [Pred] -> Instr -> (Instr', Changed?)
type FwdTransfer m a = [a] -> a -> m (a, Bool)

-- Instr -> [Succ] -> (Instr', Changed?)
type BwdTransfer m a = a -> [a] -> m (a, Bool)

-- Instr -> ([Instr'], Changed?)
type Alter m a = a -> m ([a], Bool)

-- Do the given backward analysis until fix point is reached.
iterBwd :: Monad m => BwdTransfer m a -> FlowGraph a -> m (FlowGraph a)
iterBwd transferFunc g = go transferFunc exitBlockIds g
  where
    exitBlockIds = filter (hasNoSucc g) (Map.keys (blockMap g))

    go :: Monad m => BwdTransfer m a ->
          [BlockId] -> FlowGraph a -> m (FlowGraph a)
    go trans workList g = case workList of
      [] -> return g
      bid:rest -> do
        let b = getBlock bid g
            succBlocks = map (flip getBlock g)
                             (Set.toList (getSuccBlockIds bid g))
            succInstrs = map firstInstrOfBlock succBlocks
        (b', changed) <- recalculateBwd trans b succInstrs
        if changed || null (instrList b)
          then go trans (rest ++ (Set.toList (getPredBlockIds bid g)))
                        (putBlock b' g)
          else go trans rest g

    recalculateBwd :: Monad m => BwdTransfer m a ->
                      BasicBlock a -> [a] -> m (BasicBlock a, Bool)
    recalculateBwd trans b succs = (`runStateT` False) $
      mapM (worker trans succs) b

    worker :: Monad m => BwdTransfer m a -> [a] -> a -> StateT Bool m a
    worker trans succs a = StateT $ \prevChanged -> do
      (a', changed) <- trans a succs
      return (a', prevChanged || changed)

