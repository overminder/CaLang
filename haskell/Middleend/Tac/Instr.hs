module Middleend.Tac.Instr (
  Instr(..),
) where

import Text.PrettyPrint

import Backend.Operand
import Backend.Class
import Utils.Class

instance Instruction Instr where
  isBranchInstr = ir_isBranchInstr
  localBranchTargets = ir_localBranchTargets
  isLabelInstr = ir_isLabelInstr
  getLabelOfInstr = ir_getLabelOfInstr
  isFallThroughInstr = ir_isFallThroughInstr
  mkJumpInstr = ir_mkJumpInstr
  renameBranchInstrLabel = ir_renameBranchInstrLabel

instance Ppr Instr where
  ppr = ppr_instr

-- Design decisions:
--   * Shall we allow memory operand in IR instrutions or shall we
--     use typed IR instead?
--   * Intel or AT&T syntax (lol)?
--   * Other invariants: cond op shall occur in the code or not?
data Instr
  = LABEL         Imm
  | PROLOG
  | EPILOG

  | MOV           Operand Operand
  | BINOP  MachOp Operand Operand Operand
  | UNROP  MachOp Operand Operand

  | LOAD  OpWidth Operand Operand -- dest <- [src]
  | STORE OpWidth Operand Operand -- [src] <- dest

  | CASEJUMP      Operand [Imm]
  | RET           (Maybe Operand)
  | JIF    MachOp (Operand, Operand) Imm Imm -- cmpOp 2xOps ifTrue ifFalse
  | JMP           Imm
  | CALL   CConv  Operand Operand [Operand] -- retval func args

type CConv = [CallingConv]

ppr_instr :: Instr -> Doc
ppr_instr i = case i of
  LABEL lbl -> pprImm lbl <> colon
  PROLOG -> text "prologue"
  EPILOG -> text "epilogue"
  MOV o1 o2 -> ppr o1 <+> text ":=" <+> ppr o2
  BINOP mop o1 o2 o3 -> ppr o1 <+> text ":=" <+> ppr o2 <+>
                        ppr_rator mop <+> ppr o3
  UNROP mop o1 o2 -> ppr o1 <+> text ":=" <+> ppr_rator mop <+> ppr o2
  LOAD w o1 o2 -> ppr o1 <+> text ":=" <+> text "load" <> ppr_width w <+>
                  brackets (ppr o2)
  STORE w o1 o2 -> brackets (ppr o1) <+> text ":=" <+> text "store" <>
                   ppr_width w <+> ppr o2
  CASEJUMP o lbls -> text "casejmp" <+> ppr_operands [o] <+> text "# TO" <+>
                     brackets (hcat (punctuate comma (map pprImm lbls)))
  RET (Just o) -> text "ret" <+> ppr o
  RET _ -> text "ret"
  JIF rel (r1, r2) lbl1 lbl2 ->
    text "jif" <+> ppr r1 <+> text (show_rel rel) <+> ppr r2 <+>
    brackets (hcat (punctuate comma (map pprImm [lbl1, lbl2])))
  JMP lbl -> text "jmp" <+> pprImm lbl
  CALL conv o f args ->
    ppr o <+> text ":=" <+> text "call" <+> text (show conv) <+>
    ppr f <+> (parens (hcat (punctuate comma (map ppr args))))
  where
    ppr_operands operands = hcat (punctuate comma (map ppr operands))
    show_rel r = case r of
      RGe -> ">="
      RGt -> ">"
      REq -> "=="
      RNe -> "!="
      RLe -> "<="
      RLt -> "<"
    ppr_rator rator = text $ case rator of
      AAdd -> "+"
      ASub -> "-"
      AMul -> "*"
      ADiv -> "/"
      BShl -> "<<"
      _ -> error $ show rator
    ppr_width w = text $ case w of
      W8 -> "byte"
      W16 -> "word"
      W32 -> "dword"
      W64 -> "qword"

ir_isBranchInstr i = case i of
  CASEJUMP _ _ -> True
  RET _ -> True
  JIF _ _ _ _ -> True
  JMP _ -> True
  CALL _ _ _ _ -> True
  _ -> False

ir_localBranchTargets i = case i of
  CASEJUMP _ lbls -> lbls
  RET _ -> []
  JIF _ _ o1 o2 -> [o1, o2]
  JMP i -> [i]
  CALL _ _ _ _ -> []

ir_isLabelInstr i = case i of
  LABEL _ -> True
  _ -> False

ir_getLabelOfInstr (LABEL imm) = imm

ir_isFallThroughInstr i = case i of
  CALL _ _ _ _ -> True
  _ -> False

ir_mkJumpInstr = JMP

ir_renameBranchInstrLabel f i = case i of
  JMP o -> JMP (f o)
  JIF m ops i1 i2 -> JIF m ops (f i1) (f i2)
  CASEJUMP op is -> CASEJUMP op (map f is)
  _ -> i

