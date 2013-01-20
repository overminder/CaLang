{-# LANGUAGE RankNTypes #-}
module Utils.Class (
  module Text.PrettyPrint,
  Ppr(..),
  pass,
  mapRM,
  mkBitmap,
) where

import Control.Applicative
import Control.Monad
import Data.Traversable
import Text.PrettyPrint

class Ppr a where
  ppr :: a -> Doc

pass :: forall m. Monad m => m ()
pass = return ()

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

mkBitmap :: [Bool] -> Int
mkBitmap bs = go bs 0
  where
    go bs curr = case bs of
      x:xs -> go xs (boolToBit x + curr * 2)
      [] -> curr

    boolToBit x = if x then 1 else 0
