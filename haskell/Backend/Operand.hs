module Backend.Operand (
  OpClass(..),
  OpWidth(..), opWidths,
  GcFlag(..),
  RegId(..),
  Reg(..),
  Imm(..),
  Addr(..),
  Operand(..),
  CallingConv(..), parseCallingConv,

  pprReg,
  pprWidth,
  pprCallingConvs,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint
import Data.List.Utils (split)

import Backend.Class
import qualified Backend.HOST_ARCH.Operand as Arch
import Utils.Unique
import Utils.Class

data Reg
  = VReg RegId OpClass OpWidth GcFlag
  | PReg Arch.Reg
  deriving (Show, Eq, Ord)

instance Register Reg where
  isVirtualReg (PReg _) = True
  isVirtualReg _ = False
  mkRegFromString = fmap PReg . mkRegFromString
  getRegName r = case r of
    VReg (MkRegId i) cl w gcf -> '%':show i
    PReg pr -> getRegName pr

data Imm
  = IntVal Integer
  | FloatVal Double
  | NamedLabel String
  | TempLabel String Unique
  deriving (Show, Eq, Ord)

data Addr
  = MkAddr {
    baseOfAddr :: Maybe Reg,
    indexOfAddr :: (Maybe Reg, Maybe Int),
    dispOfAddr :: Maybe Imm
  }
  deriving (Show)

data Operand
  = OpReg Reg
  | OpImm Imm
  | OpAddr Addr
  deriving (Show)

data CallingConv
  = TailCall
  | Vararg
  deriving (Show, Eq, Ord)

callingConvNames = Map.fromList [("tail", TailCall),
                                 ("vararg", Vararg)]
parseCallingConv :: String -> Maybe [CallingConv]
parseCallingConv s = mapM parse_one (split "/" s)
  where
    parse_one s = Map.lookup s callingConvNames

-- PPR
instance Ppr Operand where
  ppr op = case op of
    OpReg r -> pprReg r
    OpImm i -> pprImm i
    OpAddr a -> undefined

pprReg :: Reg -> Doc
pprReg = text . getRegName

pprImm :: Imm -> Doc
pprImm i = case i of
  IntVal iVal -> integer iVal
  NamedLabel s -> text s
  TempLabel s t -> text $ ".L" ++ s ++ show t

pprWidth w = case w of
  W8  -> int 8
  W16 -> int 16
  W32 -> int 32
  W64 -> int 64

pprCallingConvs cs = if null cs
  then empty
  else doubleQuotes (hcat (punctuate (char '/') (map ppr_calling_conv cs)))
  where
    ppr_calling_conv c = case c of
      TailCall -> text "tail"
      Vararg -> text "vararg"


