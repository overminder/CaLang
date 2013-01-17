module Middleend.FlowGraph.Analysis (
  iterBwd,
  mapRM,
) where

import Prelude hiding (mapM)
import Data.Foldable
import Data.Traversable
import Control.Applicative
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
    recalculateBwd trans b succs = do
      (b, (_, changed)) <- (`runStateT` (succs, False)) $
                             mapRM (worker trans) b
      return (b, changed)

    worker :: Monad m => BwdTransfer m a -> a -> StateT ([a], Bool) m a
    worker trans a = StateT $ \(succs, prevChanged) -> do
      (a', changed) <- trans a succs
      return (a', ([a'], prevChanged || changed))

-- mapRM implementation (which was missing from Data.Traversable)
-- Using this can be used to implement mapAccum[LR] as well.

newtype RightT m a = RightT { runRightT :: m a }

instance (Monad m) => Functor (RightT m) where
  fmap f (RightT k) = RightT $ liftM f k

instance (Monad m) => Applicative (RightT m) where
  pure = RightT . return
  RightT kf <*> RightT kv = RightT $ liftM2 (flip ($)) kv kf

mapRM :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
mapRM f t = runRightT (traverse (RightT . f) t)

