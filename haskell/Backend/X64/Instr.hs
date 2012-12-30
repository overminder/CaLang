module Backend.X64.Instr (
  Instr(..),
  pprInstr,
) where

import Text.PrettyPrint

import Backend.Operand
import Utils.Class

data Instr
  = LABEL  Operand
  
  | MOV    Operand Operand
  | MOVZxQ OpWidth Operand Operand
  | MOVSxQ OpWidth Operand Operand

  | LEA    Operand Operand

  -- Arith
  | ADD    Operand Operand
  | SUB    Operand Operand
  | MUL    Operand Operand
  | DIV    Operand Operand
  | NEG    Operand

  -- Bitwise
  | AND    Operand Operand
  | OR     Operand Operand
  | XOR    Operand Operand
  | NOT    Operand

  -- Bitwise shift, first operand need to be immediate or %cl.
  | SHL    Operand Operand
  | SAR    Operand Operand
  | SHR    Operand Operand

  -- Comparisons
  | TEST   Operand Operand
  | CMP    Operand Operand
  | CMOV   MachOp  Operand Operand

  -- Stack ops
  | PUSH   Operand
  | POP    Operand

  -- Branches
  | JMP    Operand
  | JXX    Cond Imm      -- J/Jg/Jge/Jxx...
  | CALL   Operand
  | RET

type Cond = MachOp

pprInstr :: Instr -> Doc
pprInstr i = case i of
  LABEL o -> ppr o <> colon
  _ -> text pref <+> (hcat (punctuate comma (map ppr operands)))
  where
    (pref, operands) = disas i

    show_width w = case w of
      W8  -> "b"
      W16 -> "w"
      W32 -> "l"
      W64 -> "q"

    show_rel r = case r of
      RGe -> "ge"
      RGt -> "g"
      REq -> "e"
      RNe -> "ne"
      RLe -> "le"
      RLt -> "l"

    disas i = case i of
      MOV    o1 o2 -> ("mov",  [o1, o2])
      MOVZxQ w o1 o2 -> ("movz" ++ show_width w ++ "q", [o1, o2])
      MOVSxQ w o1 o2 -> ("movs" ++ show_width w ++ "q", [o1, o2])
      LEA    o1 o2 -> ("lea",  [o1, o2])
      ADD    o1 o2 -> ("add",  [o1, o2])
      SUB    o1 o2 -> ("sub",  [o1, o2])
      MUL    o1 o2 -> ("mul",  [o1, o2])
      DIV    o1 o2 -> ("div",  [o1, o2])
      NEG    o1    -> ("neg",  [o1])
      AND    o1 o2 -> ("and",  [o1, o2])
      OR     o1 o2 -> ("or",   [o1, o2])
      XOR    o1 o2 -> ("xor",  [o1, o2])
      NOT    o1    -> ("not",  [o1])
      SHL    o1 o2 -> ("shl",  [o1, o2])
      SAR    o1 o2 -> ("sar",  [o1, o2])
      SHR    o1 o2 -> ("shr",  [o1, o2])
      TEST   o1 o2 -> ("test", [o1, o2])
      CMP    o1 o2 -> ("cmp",  [o1, o2])
      CMOV r o1 o2 -> ("cmov" ++ show_rel r, [o1, o2])
      PUSH   o1    -> ("push", [o1])
      POP    o1    -> ("pop",  [o1])
      JMP    o1    -> ("jmp",  [o1])
      JXX  r o1    -> ("j" ++ show_rel r,  [OpImm o1])
      CALL   o1    -> ("call", [o1])
      RET          -> ("ret", [])

