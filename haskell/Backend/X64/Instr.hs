module Backend.X64.Instr (
  Instr(..),
  movq,
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
  getFallThroughTarget = x64_getFallThroughTarget
  mkJumpInstr = x64_mkJumpInstr
  renameBranchInstrLabel = x64_renameBranchInstrLabel
  getUseOfInstr = x64_getUseOfInstr
  getDefOfInstr = x64_getDefOfInstr
  replaceRegInInstr = x64_replaceRegInInstr

instance Ppr Instr where
  ppr = ppr_instr

-- AT&T Syntax.
data Instr
  -- Pseudo instrs
  = LABEL  Imm
  | PROLOG
  | EPILOG
  
  | MOV    OpWidth Operand Operand
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
  | JXX    Cond Imm      -- J/Jg/Jge/Jxx...
  | CALL   Operand CallSiteInfo -- The same as JMP
  | RET    Bool -- True when returning the rax, or false otherwise
  deriving (Show)

-- Commonly used
movq = MOV W64

type CallSiteInfo = ( Bool       -- Needs result?
                    , [Operand]  -- Arguments used (%rdi-%r9, mems..)
                    )

type Cond = MachOp

-- Instruction instance impl

x64_isBranchInstr i = case i of
  JMP  _ _   -> True
  JXX  _ _   -> True
  CALL _ _   -> True
  RET _      -> True
  _          -> False

x64_isFallThroughInstr i = case i of
  JMP  _ _   -> False
  JXX  _ _   -> True
  CALL _ _   -> True
  RET _      -> False
  _          -> error $ "x64_isFallThroughInstr: not a branch instr"

x64_getFallThroughTarget i = case i of
  _          -> error $ "x64_getFallThroughTarget: not implemented"

x64_localBranchTargets i = case i of
  JMP op Nothing -> case op of
    OpImm t@(TempLabel _ _) -> [t]
    _ -> []
  JXX _ imm -> [imm]
  CALL _ _ -> []
  RET _ -> []
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
  _ -> instr

x64_getUseOfInstr instr = case instr of
  MOV    _ o1 o2 -> mkUseOfSrc o1 ++ mkUseOfDest o2
  MOVZxQ _ o1 o2 -> mkUseOfSrc o1 ++ mkUseOfDest o2
  MOVSxQ _ o1 o2 -> mkUseOfSrc o1 ++ mkUseOfDest o2

  LEA      o1 o2 -> mkUseOfSrc o1 ++ mkUseOfDest o2

  ADD      o1 o2 -> mkUseOfSrc o1 ++ mkUseOfSrc  o2 ++ mkUseOfDest o2
  SUB      o1 o2 -> mkUseOfSrc o1 ++ mkUseOfSrc  o2 ++ mkUseOfDest o2
  MUL      o1 o2 -> mkUseOfSrc o1 ++ mkUseOfSrc  o2 ++ mkUseOfDest o2
  DIV      o1 o2 -> mkUseOfSrc o1 ++ mkUseOfSrc  o2 ++ mkUseOfDest o2
  NEG      o1    -> mkUseOfSrc o1 ++ mkUseOfDest o1

  AND      o1 o2 -> mkUseOfSrc o1 ++ mkUseOfSrc  o2 ++ mkUseOfDest o2
  OR       o1 o2 -> mkUseOfSrc o1 ++ mkUseOfSrc  o2 ++ mkUseOfDest o2
  XOR      o1 o2 -> if False --o1 == o2
                      then []
                      else mkUseOfSrc o1 ++ mkUseOfDest o2
  NOT      o1    -> mkUseOfSrc o1 ++ mkUseOfDest o1

  SHL      o1 o2 -> mkUseOfSrc o1 ++ mkUseOfSrc  o2 ++ mkUseOfDest o2
  SAR      o1 o2 -> mkUseOfSrc o1 ++ mkUseOfSrc  o2 ++ mkUseOfDest o2
  SHR      o1 o2 -> mkUseOfSrc o1 ++ mkUseOfSrc  o2 ++ mkUseOfDest o2

  TEST     o1 o2 -> mkUseOfSrc o1 ++ mkUseOfSrc  o2
  CMP      o1 o2 -> mkUseOfSrc o1 ++ mkUseOfSrc  o2
  CMOV   _ o1 o2 -> mkUseOfSrc o1 ++ mkUseOfDest o2

  PUSH     o1    -> mkUseOfSrc o1
  POP         o1 ->                  mkUseOfDest o1

  JMP      o1 mc -> mkUseOfSrc o1 ++ maybe [] mkUseOfCallSiteInfo mc
  JXX      _  _  -> []
  CALL     o1 c  -> mkUseOfSrc o1 ++ mkUseOfCallSiteInfo c
  RET      useAx -> if useAx then [rax] else []
  _              -> []

x64_getDefOfInstr instr = case instr of
  MOV    _ o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2
  MOVZxQ _ o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2
  MOVSxQ _ o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2

  LEA      o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2

  ADD      o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2
  SUB      o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2
  MUL      o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2
  DIV      o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2
  NEG      o1    -> mkDefOfSrc o1 ++ mkDefOfDest o1

  AND      o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2
  OR       o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2
  XOR      o1 o2 -> if o1 == o2
                      then []
                      else mkDefOfSrc o1 ++ mkDefOfDest o2
  NOT      o1    -> mkDefOfSrc o1 ++ mkDefOfDest o1

  SHL      o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2
  SAR      o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2
  SHR      o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2

  TEST     o1 o2 -> []
  CMP      o1 o2 -> []
  CMOV   _ o1 o2 -> mkDefOfSrc o1 ++ mkDefOfDest o2

  PUSH     o1    -> mkDefOfSrc o1
  POP         o1 ->                  mkDefOfDest o1

  JMP      o1 mc -> mkDefOfSrc o1 ++ maybe [] mkDefOfCallSiteInfo mc
  JXX      _  _  -> []
  CALL     o1 c  -> mkDefOfSrc o1 ++ mkDefOfCallSiteInfo c
  RET      _     -> []
  _              -> []

-- XXX: not considering args passed by mem ATM
mkUseOfCallSiteInfo (_, args) = map unReg args
  where
    unReg (OpReg r) = r

mkDefOfCallSiteInfo (hasResult, _) = if hasResult then [rax] else []

x64_replaceRegInInstr f i = case i of
  MOV    w o1 o2 -> MOV    w (f' o1) (f' o2)
  MOVZxQ w o1 o2 -> MOVZxQ w (f' o1) (f' o2)
  MOVSxQ w o1 o2 -> MOVSxQ w (f' o1) (f' o2)

  LEA      o1 o2 -> LEA      (f' o1) (f' o2)

  ADD      o1 o2 -> ADD      (f' o1) (f' o2)
  SUB      o1 o2 -> SUB      (f' o1) (f' o2)
  MUL      o1 o2 -> MUL      (f' o1) (f' o2)
  DIV      o1 o2 -> DIV      (f' o1) (f' o2)
  NEG      o1    -> NEG      (f' o1)

  AND      o1 o2 -> AND      (f' o1) (f' o2)
  OR       o1 o2 -> OR       (f' o1) (f' o2)
  XOR      o1 o2 -> XOR      (f' o1) (f' o2)
  NOT      o1    -> NOT      (f' o1)

  SHL      o1 o2 -> SHL      (f' o1) (f' o2)
  SAR      o1 o2 -> SAR      (f' o1) (f' o2)
  SHR      o1 o2 -> SHR      (f' o1) (f' o2)

  TEST     o1 o2 -> TEST     (f' o1) (f' o2)
  CMP      o1 o2 -> CMP      (f' o1) (f' o2)
  CMOV   r o1 o2 -> CMOV r   (f' o1) (f' o2)

  PUSH     o1    -> PUSH     (f' o1)
  POP      o1    -> POP      (f' o1)

  JMP      o1 mc -> JMP      (f' o1) mc
  JXX      _  _  -> i 
  CALL     o1 c  -> CALL     (f' o1) c
  RET      _     -> i
  _              -> i
  where
    f' = replaceRegInOp f

-- Ppr impl

ppr_instr :: Instr -> Doc
ppr_instr i = case i of
  LABEL i -> pprImm i <> colon
  PROLOG  -> text "prolog"
  EPILOG  -> text "epilog"
  _ -> text pref <+> (hcat (punctuate comma (map (pprGasOperand i) operands)))
  where
    pprGasOperand i op = case op of
      OpImm imm -> immPrefix <> pprImm imm
      OpReg r -> regPrefix <> pprReg r
      OpAddr a -> addrPrefix <> pprAddr a
      where
        immPrefix = case i of
          JMP  _ _ -> empty
          JXX  _ _ -> empty
          CALL _ _ -> empty
          _        -> char '$'

        regPrefix = case i of
          JMP  _ _ -> char '*'
          CALL _ _ -> char '*'
          _        -> empty

        addrPrefix = regPrefix

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
      MOV    w o1 o2 -> ("mov" ++ show_width w,  [o1, o2])
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
      _ -> error $ "disas: " ++ show i

