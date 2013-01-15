module Backend.Operand (
  module Backend.Common,
  module Backend.HOST_ARCH.Regs,

  -- Pretty printers
  pprReg, pprImm, pprAddr,
  pprWidth,
  pprCallingConvs,
) where

import Data.Tuple (swap)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint

import Backend.Common
import Backend.HOST_ARCH.Regs
import Utils.Class

instance Register Reg where
  isPhysicalReg r = case r of
    RegV rv -> isPhysicalReg rv
    RegP rp -> isPhysicalReg rp

  mkRegFromString = fmap RegP . mkRegFromString

  getRegName r = case r of
    RegV rv -> getRegName rv
    RegP rp -> getRegName rp

  getClassOfReg r = case r of
    RegV rv -> getClassOfReg rv
    RegP rp -> getClassOfReg rp

-- PPR
instance Ppr Operand where
  ppr op = case op of
    OpReg r -> pprReg r
    OpImm i -> pprImm i
    OpAddr a -> pprAddr a

pprReg :: Register r => r -> Doc
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
    ppr_calling_conv c = text (callingConvToName Map.! c)
    callingConvToName = Map.fromList (map swap (Map.toList callingConvNames))

