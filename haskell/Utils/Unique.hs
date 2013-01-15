{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts,
             UndecidableInstances #-}
module Utils.Unique (
  Unique,
  MonadUnique,
  liftU,
  mkUnique,
  UniqueM,
  evalUniqueM
) where

import Control.Monad.Trans

type Unique = Int

newtype UniqueM a = UniqueM { runUniqueM :: Unique -> (a, Unique) }

evalUniqueM m = fst . runUniqueM m $ 0

instance Monad UniqueM where
  return a = UniqueM $ \s -> (a, s)
  m >>= k  = UniqueM $ \s -> let (a, s') = runUniqueM m s
                              in runUniqueM (k a) s'

-- liftU
class (Monad m) => MonadUnique m where
  liftU :: UniqueM a -> m a

instance MonadUnique UniqueM where
  liftU = id

instance (MonadUnique m, MonadTrans t, Monad (t m)) => MonadUnique (t m) where
  liftU = lift . liftU

mkUnique :: MonadUnique m => m Unique
mkUnique = liftU (UniqueM $ \i -> (i, i+1))

