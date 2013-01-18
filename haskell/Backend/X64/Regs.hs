module Backend.X64.Regs (
  generalRegs,
  returnReg,
  framePtrReg, stackPtrReg,

  rax, rcx, rdx, rbx, rdi, rsi, rsp, rbp, r8,
  r9, r10, r11, r12, r13, r14, r15, rip,
  eax, ecx, edx, ebx, edi, esi, esp, ebp, r8d,
  r9d, r10d, r11d, r12d, r13d, r14d, r15d, eip,
  cl,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint

import Backend.Common

instance Register PhysicalReg where
  isPhysicalReg _ = True
  mkRegFromString ('%':name) = fmap unRegP (Map.lookup name regNameToReg)
  getRegName (PhysicalReg (MkRegId i, _, w, gcf)) = case w of
    W64 -> "%" ++ quadRegNames !! i
    W32 -> "%" ++ dwordRegNames !! i
    W16 -> "%" ++ wordRegNames !! i
    W8  -> "%" ++ byteRegNames !! i
  getClassOfReg (PhysicalReg (_, kls, w, gcf)) = (kls, w, gcf)

unRegP (RegP rp) = rp

quadRegNames = words ("rax rcx rdx rbx rdi rsi rsp rbp r8 r9 r10 r11 " ++
                      "r12 r13 r14 r15 rip")

dwordRegNames = words ("eax ecx edx ebx edi esi esp ebp r8d r9d r10d r11d " ++
                       "r12d r13d r14d r15d eip")

wordRegNames = words ("ax cx dx bx di si sp bp r8w r9w r10w r11w " ++
                      "r12w r13w r14w r15w ip")

byteRegNames = words ("al cl dl bl dil sil spl bpl r8b r9b r10b r11b " ++
                      "r12b r13b r14b r15b ipl")

quadRegs = mkRegs W64 quadRegNames
[rax, rcx, rdx, rbx, rdi, rsi, rsp, rbp, r8,
 r9, r10, r11, r12, r13, r14, r15, rip] = quadRegs

dwordRegs = mkRegs W32 dwordRegNames
[eax, ecx, edx, ebx, edi, esi, esp, ebp, r8d,
 r9d, r10d, r11d, r12d, r13d, r14d, r15d, eip] = dwordRegs

wordRegs = mkRegs W16 wordRegNames
[ax, cx, dx, bx, di, si, sp, bp, r8w,
 r9w, r10w, r11w, r12w, r13w, r14w, r15w, ip] = wordRegs

byteRegs = mkRegs W8 byteRegNames
[al, cl, dl, bl, dil, sil, spl, bpl, r8b,
 r9b, r10b, r11b, r12b, r13b, r14b, r15b, ipl] = byteRegs

mkRegs w names = map mk_reg (zip [0..] names)
  where
    mk_reg (i, _) = RegP (PhysicalReg (MkRegId i, SignedOp, w, MkGcFlag False))

regNameToReg = Map.fromList ((zip quadRegNames quadRegs) ++
                             (zip dwordRegNames dwordRegs))

generalRegs = reverse [rbx, r12, r13, r14, r15,
               rax, rcx, rdx, rdi, rsi, r8, r9, r10, r11]

returnReg = rax
framePtrReg = rbp
stackPtrReg = rsp
