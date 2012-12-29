module Backend.Class (
  Register(..),
  OpClass(..),
  OpWidth(..), opWidths, intToWidth,
  GcFlag(..),
  RegId(..),
) where

class Register r where
  isVirtualReg :: r -> Bool
  mkRegFromString :: String -> Maybe r
  getRegName :: r -> String

data OpClass
  = FloatingOp
  | SignedOp
  | UnsignedOp
  deriving (Show, Eq, Ord)

data OpWidth
  = W8
  | W16
  | W32
  | W64
  deriving (Show, Eq, Ord)

opWidths = [W8, W16, W32, W64]
intToWidth i = case i of
  8  -> W8
  16 -> W16
  32 -> W32
  64 -> W64
  _ -> error $ "Class.intToWidth: no such width: " ++ show i

newtype GcFlag
  = MkGcFlag { getGcFlag :: Bool }
  deriving (Show, Eq, Ord)

newtype RegId
  = MkRegId { getRegId :: Int }
  deriving (Show, Eq, Ord)

