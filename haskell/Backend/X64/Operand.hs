module Backend.X64.Operand (
  Reg(..)
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint

import Backend.Class

data Reg
  = Reg RegId OpClass OpWidth GcFlag
  deriving (Show, Eq, Ord)

instance Register Reg where
  isVirtualReg _ = False
  mkRegFromString ('%':name) = Map.lookup name regNameToReg
  getRegName (Reg (MkRegId i) _ _ _) = '%':regNames !! i

regNames = words ("rax rcx rdx rbx rdi rsi rsp rbp r8 r9 r10 r11 r12 r13" ++
                  "r14 r15 rip")

regs = map mk_reg (zip [0..] regNames)
  where
    mk_reg (i, _) = Reg (MkRegId i) SignedOp W64 (MkGcFlag False)

regNameToReg = Map.fromList (zip regNames regs)

