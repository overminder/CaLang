module Middleend.FlowGraph.Analysis (
  iterAnalysis,
  Fwd(..), Bwd(..), Alter(..),
) where

import Prelude hiding (mapM, concat, foldr)
import Control.Applicative
import Control.Monad.State hiding (forM, forM_, mapM, mapM_)
import Data.Maybe
import Data.Foldable
import Data.Traversable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Middleend.FlowGraph.Builder
import Utils.Class

-- [Pred] -> Instr -> (Instr', Changed?)
type FwdTransfer m a = [a] -> a -> m (a, Bool)

-- Instr -> [Succ] -> (Instr', Changed?)
type BwdTransfer m a = a -> [a] -> m (a, Bool)

-- Instr -> ([Instr'], Changed?)
type AlterTransfer m a = a -> m ([a], Bool)

newtype Fwd m a = Fwd { runFwd :: FwdTransfer m a }

newtype Bwd m a = Bwd { runBwd :: BwdTransfer m a }

newtype Alter m a = Alter { runAlter :: AlterTransfer m a }

class TransferFunc f where
  mkInitialWorkList :: f m a -> FlowGraph a -> [BlockId]
  mkNewWorks :: f m a -> FlowGraph a -> BlockId -> [BlockId]
  inputInstr :: f m a -> FlowGraph a -> BlockId -> [a]
  analyzeBlock :: Monad m => f m a -> BasicBlock a ->
                  [a] -> m (BasicBlock a, Bool)

instance TransferFunc Bwd where
  mkInitialWorkList _ g = Map.keys (blockMap g)
  mkNewWorks _ g bid = Set.toList (getPredBlockIds bid g)
  inputInstr _ g bid =
    let b = getBlock bid g
        succBlocks = map (flip getBlock g)
                         (Set.toList (getSuccBlockIds bid g))
     in map firstInstrOfBlock succBlocks
  analyzeBlock f b succs = do
    (b', (_, changed)) <- (`runStateT` (succs, False)) $
                           mapRM (bwdWorker (runBwd f)) b
    return (b', changed)

instance TransferFunc Fwd where
  mkInitialWorkList _ g = Map.keys (blockMap g)
  mkNewWorks _ g bid = Set.toList (getSuccBlockIds bid g)
  inputInstr _ g bid =
    let b = getBlock bid g
        predBlocks = map (flip getBlock g)
                         (Set.toList (getPredBlockIds bid g))
     in map lastInstrOfBlock predBlocks
  analyzeBlock f b preds = do
    (b', (_, changed)) <- (`runStateT` (preds, False)) $
                           mapM (fwdWorker (runFwd f)) b
    return (b', changed)

instance TransferFunc Alter where
  mkInitialWorkList _ g = Map.keys (blockMap g)
  mkNewWorks _ _ _ = []
  inputInstr _ _ _ = []
  analyzeBlock f b _ = do
    (b', changed) <- (`runStateT` False) $ mapM (alterWorker (runAlter f)) b
    return (MkBlock {
      blockId = blockId b',
      instrList = concat (instrList b'),
      controlInstr = join (fmap listToMaybe (controlInstr b')),
      blockLabels = blockLabels b'
    }, changed)

-- Do the given (for|back)ward analysis until fix point is reached.
iterAnalysis :: (Monad m, TransferFunc f) =>
                f m a -> FlowGraph a -> m (FlowGraph a, Bool)
iterAnalysis f g = go (mkInitialWorkList f g) g False
  where
    go workList g changed = case workList of
      [] -> return (g, changed)
      bid:rest -> do
        let b = getBlock bid g
            initialInput = inputInstr f g bid
        (b', changed') <- analyzeBlock f b initialInput
        let combinedChange = changed || changed'
        if changed' || null (instrList b)
          then go (rest ++ mkNewWorks f g bid) (putBlock b' g) combinedChange
          else go rest g combinedChange

fwdWorker :: Monad m => FwdTransfer m a -> a -> StateT ([a], Bool) m a
fwdWorker trans a = StateT $ \(preds, prevChanged) -> do
  (a', changed) <- trans preds a
  return (a', ([a'], prevChanged || changed))

bwdWorker :: Monad m => BwdTransfer m a -> a -> StateT ([a], Bool) m a
bwdWorker trans a = StateT $ \(succs, prevChanged) -> do
  (a', changed) <- trans a succs
  return (a', ([a'], prevChanged || changed))

alterWorker :: Monad m => AlterTransfer m a -> a -> StateT Bool m [a]
alterWorker trans a = StateT $ \prevChanged -> do
  (as', changed) <- trans a
  return (as', prevChanged || changed)

