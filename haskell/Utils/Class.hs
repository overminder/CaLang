module Utils.Class where

import Text.PrettyPrint

class Ppr a where
  ppr :: a -> Doc

