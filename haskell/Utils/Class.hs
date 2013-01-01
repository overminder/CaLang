{-# LANGUAGE RankNTypes #-}
module Utils.Class where

import Text.PrettyPrint

class Ppr a where
  ppr :: a -> Doc

pass :: forall m. Monad m => m ()
pass = return ()

