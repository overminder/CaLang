module Backend.X64.Instr (
  Instr(..),
) where

import Text.PrettyPrint

import Backend.Operand
import Utils.Class

instance Instruction Instr where
  isBranchInstr = x64_isBranchInstr
  localBranchTargets = x64_localBranchTargets
  isLabelInstr = x64_isLabelInstr
  getLabelOfInstr = x64_getLabelOfInstr
  isFallThroughInstr = x64_isFallThroughInstr
  mkJumpInstr = x64_mkJumpInstr
  renameBranchInstrLabel = x64_renameBranchInstrLabel
  getUseOfInstr = x64_getUseOfInstr
  getDefOfInstr = x64_getDefOfInstr

instance Ppr Instr where
  ppr = ppr_instr

x64_isBranchInstr i = case i of
  JMP  _ _   -> True
  JXX  _ _   -> True
  CALL _ _   -> True
  RET _      -> True
  SWITCH _ _ -> True
  _          -> False

x64_isFallThroughInstr i = case i of
  JMP  _ _   -> False
  JXX  _ _   -> True
  CALL _ _   -> True
  RET _      -> False
  SWITCH _ _ -> False
  _          -> error $ "x64_isFallThroughInstr: not a branch instr"

x64_localBranchTargets i = case i of
  JMP op Nothing -> case op of
    OpImm t@(TempLabel _ _) -> [t]
    _ -> []
  JXX _ imm -> [imm]
  CALL _ _ -> []
  RET _ -> []
  SWITCH _ lbls -> lbls
  _ -> error $ "x64_localBranchTarget: not a branch instr"

x64_isLabelInstr i = case i of
  LABEL _ -> True
  _ -> False

x64_getLabelOfInstr i = case i of
  LABEL imm -> imm
  _ -> error $ "x64_getLabelOfInstr: not a label instr"

x64_mkJumpInstr = flip JMP Nothing . OpImm

x64_renameBranchInstrLabel f instr = case instr of
  JMP (OpImm i) Nothing -> JMP (OpImm (f i)) Nothing
  JXX c i -> JXX c (f i)
  SWITCH op is -> SWITCH op (map f is)
  _ -> instr

x64_getUseOfInstr instr = undefined
x64_getDefOfInstr instr = undefined

-- AT&T Syntax.
data Instr
  -- Pseudo instrs
  = LABEL  Imm
  | PROLOG
  | EPILOG
  
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
  | JMP    Operand (Maybe CallSiteInfo) -- callsite info is for tailcall
  | SWITCH Operand [Imm] -- jump to multiple basic blocks
  | JXX    Cond Imm      -- J/Jg/Jge/Jxx...
  | CALL   Operand CallSiteInfo -- The same as JMP
  | RET    Bool -- True when returning the rax, or false otherwise
  deriving (Show)

type CallSiteInfo = ( Bool       -- Needs result?
                    , [Operand]  -- Arguments used (%rdi-%r9, mems..)
                    )

type Cond = MachOp

ppr_instr :: Instr -> Doc
ppr_instr i = case i of
  LABEL i -> pprImm i <> colon
  SWITCH o lbls -> ppr_instr (JMP o Nothing) <+> text "# SWITCH =>" <+>
                   brackets (hcat (punctuate comma (map pprImm lbls)))
  _ -> text pref <+> (hcat (punctuate comma (map pprGasOperand operands)))
  where
    pprGasOperand op = case op of
      OpImm (IntVal i) -> char '$' <> integer i
      _ -> ppr op
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
      JMP    o1 _  -> ("jmp",  [o1])
      JXX  r o1    -> ("j" ++ show_rel r,  [OpImm o1])
      CALL   o1 _  -> ("call", [o1])
      RET    _     -> ("ret", [])

