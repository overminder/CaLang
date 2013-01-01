module Backend.X64.Regs (
  argRegs,
  returnReg,
  rax, rcx, rdx, rbx, rdi, rsi, rsp, rbp, r8,
  r9, r10, r11, r12, r13, r14, r15, rip,
) where

import Backend.Operand
import Backend.Class

regNames = words ("rax rcx rdx rbx rdi rsi rsp rbp r8 r9 r10 r11 r12 r13 " ++
                  "r14 r15 rip")

instance Register PhysicalReg where
  isVirtualReg _ = False
  mkRegFromString ('%':name) = Map.lookup name regNameToReg
  getRegName (Reg (MkRegId i) _ _ _) = '%':regNames !! i
  getClassOfReg (Reg _ c w f) = (c, w, f)

regs = map mk_reg (zip [0..] regNames)
  where
    mk_reg (i, _) = MkPReg (MkRegId i) SignedOp W64 (MkGcFlag False)

regNameToReg = Map.fromList (zip regNames regs)

[rax, rcx, rdx, rbx, rdi, rsi, rsp, rbp, r8,
 r9, r10, r11, r12, r13, r14, r15, rip] = regs

argRegs = [rdi, rsi, rdx, rcx, r8, r9]
returnReg = rax

