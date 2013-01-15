module Middleend.Tac.Instr (
  Instr(..),
) where

import Data.Foldable
import Prelude hiding (concat, notElem)
import Text.PrettyPrint

import Backend.Operand
import Utils.Class

instance Instruction Instr where
  isBranchInstr = ir_isBranchInstr
  localBranchTargets = ir_localBranchTargets
  isLabelInstr = ir_isLabelInstr
  getLabelOfInstr = ir_getLabelOfInstr
  isFallThroughInstr = ir_isFallThroughInstr
  getFallThroughTarget = ir_getFallThroughTarget
  mkJumpInstr = ir_mkJumpInstr
  renameBranchInstrLabel = ir_renameBranchInstrLabel
  getUseOfInstr = ir_getUseOfInstr
  getDefOfInstr = ir_getDefOfInstr
  replaceRegInInstr = ir_replaceRegInInstr

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

  | MOV           Reg Operand
  | BINOP  MachOp Reg Operand Operand
  | UNROP  MachOp Reg Operand

  | LOAD  OpWidth Reg Operand -- dest <- [src]
  | STORE OpWidth Reg Operand -- [dest] <- src

  | CASEJUMP      Operand [Imm]
  | RET           (Maybe Operand)
  | JIF    MachOp (Operand, Operand) Imm Imm -- cmpOp 2xOps ifTrue ifFalse
  | JMP           Imm
  | CALL   CConv  (Maybe Reg) Operand [Operand] -- retval func args
  deriving (Show)

type CConv = [CallingConv]

ppr_instr :: Instr -> Doc
ppr_instr i = case i of
  LABEL lbl -> pprImm lbl <> colon
  PROLOG -> text "prologue"
  EPILOG -> text "epilogue"
  MOV r1 o2 -> pprReg r1 <+> text ":=" <+> ppr o2
  BINOP mop r1 o2 o3 -> pprReg r1 <+> text ":=" <+> ppr o2 <+>
                        ppr_rator mop <+> ppr o3
  UNROP mop r1 o2 -> pprReg r1 <+> text ":=" <+> ppr_rator mop <+> ppr o2
  LOAD w r1 o2 -> pprReg r1 <+> text ":=" <+> text "load" <> ppr_width w <+>
                  brackets (ppr o2)
  STORE w r1 o2 -> brackets (pprReg r1) <+> text ":=" <+> text "store" <>
                   ppr_width w <+> ppr o2
  CASEJUMP o lbls -> text "casejmp" <+> ppr_operands [o] <+> text "# TO" <+>
                     brackets (hcat (punctuate comma (map pprImm lbls)))
  RET (Just o) -> text "ret" <+> ppr o
  RET _ -> text "ret"
  JIF rel (r1, r2) lbl1 lbl2 ->
    text "jif" <+> ppr r1 <+> text (show_rel rel) <+> ppr r2 <+>
    brackets (hcat (punctuate comma (map pprImm [lbl1, lbl2])))
  JMP lbl -> text "jmp" <+> pprImm lbl
  CALL conv mo f args -> (case mo of
    Just r -> pprReg r <+> text ":="
    Nothing -> empty) <+> text "call" <+> text (show conv) <+>
    ppr f <+> (parens (hcat (punctuate comma (map ppr args))))
  where
    ppr_operands operands = hcat (punctuate comma (map ppr operands))
    show_rel op = if isCondOp op
      then showMachOp op
      else error $ "Tac.Instr.show_rel: not a relop: " ++ show op
    ppr_rator = text . showMachOp
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
  CALL conv _ _ _ -> Noret `notElem` conv &&
                     TailCall `notElem` conv
  _ -> False

ir_getFallThroughTarget i = case i of
  JIF _ _ _ o2 -> o2

ir_mkJumpInstr = JMP

ir_renameBranchInstrLabel f i = case i of
  JMP o -> JMP (f o)
  JIF m ops i1 i2 -> JIF m ops (f i1) (f i2)
  CASEJUMP op is -> CASEJUMP op (map f is)
  _ -> i

ir_getDefOfInstr instr = case instr of
  MOV r1 _ -> [r1]
  BINOP _ r1 _ _ -> [r1]
  UNROP _ r1 _ -> [r1]
  LOAD _ r1 _ -> [r1]
  CALL _ mbR _ _ -> toList mbR
  _ -> []

ir_getUseOfInstr instr = case instr of
  MOV _ o2 -> to_reg [o2]
  BINOP _ _ o2 o3 -> to_reg [o2, o3]
  UNROP _ _ o2 -> to_reg [o2]
  LOAD _ _ o2 -> to_reg [o2]
  STORE _ r1 o2 -> r1 : to_reg [o2]
  CASEJUMP o1 _ -> to_reg [o1]
  RET mOp -> to_reg (toList mOp)
  JIF _ (o1, o2) _ _ -> to_reg [o1, o2]
  CALL _ _ f args -> to_reg (f:args)
  _ -> []
  where
    to_reg ops = concat (map as_reg ops)
    as_reg op = case op of
      OpReg r -> [r]
      _ -> []

ir_replaceRegInInstr _ _ = error $ "ir_replaceRegInInstr: not implemented"

