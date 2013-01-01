module Backend.X64.Operand (
  Reg(..),
  argRegs,
  returnReg,
  rax, rcx, rdx, rbx, rdi, rsi, rsp, rbp, r8,
  r9, r10, r11, r12, r13, r14, r15, rip,
  _PReg_isVirtualReg,
  _PReg_mkRegFromString,
  _PReg_getRegName,
  _PReg_getClassOfReg,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint

import Backend.Common

data Reg
  = Reg RegId OpClass OpWidth GcFlag
  deriving (Show, Eq, Ord)

_PReg_isVirtualReg _ = False
_PReg_mkRegFromString ('%':name) = Map.lookup name regNameToReg
_PReg_getRegName (Reg (MkRegId i) _ _ _) = '%':regNames !! i
_PReg_getClassOfReg (Reg _ c w f) = (c, w, f)

regNames = words ("rax rcx rdx rbx rdi rsi rsp rbp r8 r9 r10 r11 r12 r13 " ++
                  "r14 r15 rip")

[rax, rcx, rdx, rbx, rdi, rsi, rsp, rbp, r8,
 r9, r10, r11, r12, r13, r14, r15, rip] = regs

argRegs = [rdi, rsi, rdx, rcx, r8, r9]
returnReg = rax

regs = map mk_reg (zip [0..] regNames)
  where
    mk_reg (i, _) = Reg (MkRegId i) SignedOp W64 (MkGcFlag False)

regNameToReg = Map.fromList (zip regNames regs)


