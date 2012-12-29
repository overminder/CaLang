module Utils.Unique (
  Unique,
  UniqueM,
  mkUnique,
  runUniqueM
) where

import Control.Monad.State

type Unique = Int

type UniqueM = State Unique

mkUnique :: UniqueM Unique
mkUnique = do
  i <- get
  put $ i + 1
  return i

runUniqueM :: UniqueM a -> a
runUniqueM m = evalState m 0

