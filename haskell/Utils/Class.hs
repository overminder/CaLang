{-# LANGUAGE RankNTypes #-}
module Utils.Class (
  module Text.PrettyPrint,
  Ppr(..),
  pass,
  mapRM,
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
