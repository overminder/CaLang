module Backend.Operand (
  MachOp(..), reverseCond, isCondOp,
  OpClass(..),
  OpWidth(..), opWidths,
  StorageType,
  GcFlag(..),
  RegId(..),
  Reg(..),
  Imm(..),
  Addr(..),
  Operand(..),
  CallingConv(..), parseCallingConv,

  -- Commonly used
  newVReg,
  newTempLabel,

  pprReg, pprImm, pprAddr,
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

-- base + disp + index(r * i)
data Addr
  = MkAddr {
    baseOfAddr :: Maybe Reg,
    indexOfAddr :: Maybe (Reg, Int),
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

-- Commonly used
newVReg :: StorageType -> UniqueM Operand
newVReg (kls, width, gc) = do
  i <- mkUnique
  return . OpReg $ VReg (MkRegId i) kls width gc

newTempLabel :: String -> UniqueM Operand
newTempLabel s = do
  i <- mkUnique
  return . OpImm . TempLabel s $ i

-- PPR
instance Ppr Operand where
  ppr op = case op of
    OpReg r -> pprReg r
    OpImm i -> pprImm i
    OpAddr a -> pprAddr a

pprReg :: Reg -> Doc
pprReg = text . getRegName

pprImm :: Imm -> Doc
pprImm i = case i of
  IntVal iVal -> integer iVal
  NamedLabel s -> text s
  TempLabel s t -> text $ ".L" ++ s ++ show t
  BlockLabel i -> text $ ".LBlock" ++ show i

pprAddr :: Addr -> Doc
pprAddr (MkAddr base index disp) = disp_str <> parens (base_str <> index_str)
  where
    base_str = case base of
      Just r -> pprReg r
      _ -> empty
    index_str = case index of
      Just (r, i) -> comma <> pprReg r <> comma <> int i
      _ -> empty
    disp_str = case disp of
      Just i -> pprImm i
      _ -> empty

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


