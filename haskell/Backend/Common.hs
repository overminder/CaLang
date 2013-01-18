module Backend.Common (
  -- Typeclasses
  Register(..), Instruction(..),

  -- Primitives
  MachOp(..), reverseCond, isCondOp, isLogicOp, showMachOp,
  OpClass(..),
  OpWidth(..), opWidths, intToWidth,
  StorageType,
  GcFlag(..),
  RegId(..),

  -- Instr Operands
  Reg(..), PhysicalReg(..), VirtualReg(..),
  Imm(..),
  Addr(..),
  Operand(..), isReg, isImm, isAddr,
  CallingConv(..), parseCallingConv, callingConvNames,
  GcMap(..),

  -- Used by codegen
  newRegV,
  newTempLabel,
  setOpWidth,
  gcFlagOfReg,

  -- Used by regalloc
  mkUseOfSrc, mkDefOfSrc,
  mkUseOfDest, mkDefOfDest,
  replaceRegInOp,
  copyGcFlag,
  copyOpWidth,

) where

import Control.Arrow
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Utils (split)

import Utils.Unique

class Register r where
  isVirtualReg :: r -> Bool
  isPhysicalReg :: r -> Bool

  isVirtualReg = not . isPhysicalReg
  isPhysicalReg = not . isVirtualReg

  mkRegFromString :: String -> Maybe r
  getRegName :: r -> String
  getClassOfReg :: r -> StorageType

class Instruction instr where
  isBranchInstr :: instr -> Bool
  localBranchTargets :: instr -> [Imm]

  isLabelInstr :: instr -> Bool
  getLabelOfInstr :: instr -> Imm

  isFallThroughInstr :: instr -> Bool
  -- Nothing if you need to find it by yourself in the flowgraph
  getFallThroughTarget :: instr -> Maybe Imm

  mkJumpInstr :: Imm -> instr
  mkNopInstr :: instr

  renameBranchInstrLabel :: (Imm -> Imm) -> instr -> instr
  getUseOfInstr :: instr -> [Reg]
  getDefOfInstr :: instr -> [Reg]
  replaceRegInInstr :: (Reg -> Reg) -> instr -> instr
  isPureInstr :: instr -> Bool
  isSimpleMoveInstr :: instr -> Bool

data Reg
  = RegV VirtualReg
  | RegP PhysicalReg
  deriving (Show, Eq, Ord)

type RegDescr = (RegId, OpClass, OpWidth, GcFlag)

newtype VirtualReg = VirtualReg { unVirtReg :: RegDescr }
  deriving (Show)

instance Eq VirtualReg where
  (VirtualReg (rid, _, _, _)) == (VirtualReg (rid', _, _, _)) = rid == rid'

instance Ord VirtualReg where
  compare (VirtualReg (rid, _, _, _)) (VirtualReg (rid', _, _, _))
    = compare rid rid'

newtype PhysicalReg = PhysicalReg { unPhysReg :: RegDescr }
  deriving (Show)

instance Eq PhysicalReg where
  (PhysicalReg (rid, _, _, _)) == (PhysicalReg (rid', _, _, _)) = rid == rid'

instance Ord PhysicalReg where
  compare (PhysicalReg (rid, _, _, _)) (PhysicalReg (rid', _, _, _))
    = compare rid rid'

instance Register VirtualReg where
  isVirtualReg _ = True
  mkRegFromString _ = undefined
  getRegName (VirtualReg (MkRegId i, kls, w, gcf))
    = "%" ++ (if getGcFlag gcf then "gcptr" else "") ++ show i
  getClassOfReg (VirtualReg (_, kls, w, gcf)) = (kls, w, gcf)

-- base + disp + index(r * i)
data Addr
  = MkAddr {
    baseOfAddr :: Maybe Reg,
    indexOfAddr :: Maybe (Reg, Int),
    dispOfAddr :: Maybe Imm
  }
  deriving (Show, Eq, Ord)

data Operand
  = OpReg Reg
  | OpImm Imm
  | OpAddr Addr
  deriving (Show, Eq, Ord)

isReg (OpReg _)   = True
isReg _           = False

isImm (OpImm _)   = True
isImm _           = False

isAddr (OpAddr _) = True
isAddr _          = False

data CallingConv
  = TailCall
  | Vararg
  | Noret
  | CCall
  deriving (Show, Eq, Ord)

callingConvNames = Map.fromList [("tail", TailCall),
                                 ("vararg", Vararg),
                                 ("C", CCall),
                                 ("noret", Noret)]

parseCallingConv :: String -> Maybe [CallingConv]
parseCallingConv s = mapM parse_one (split "/" s)
  where
    parse_one s = Map.lookup s callingConvNames

data Imm
  = IntVal Integer
  | FloatVal Double
  | NamedLabel String
  | TempLabel String Unique
  | BlockLabel Unique -- Used in flow graph
  deriving (Show, Eq, Ord)

data OpClass
  = FloatingOp
  | SignedOp
  | UnsignedOp
  deriving (Show, Eq, Ord)

data OpWidth
  = W8
  | W16
  | W32
  | W64
  deriving (Show, Eq, Ord)

type StorageType = (OpClass, OpWidth, GcFlag)

opWidths = [W8, W16, W32, W64]
intToWidth i = case i of
  8  -> W8
  16 -> W16
  32 -> W32
  64 -> W64
  _ -> error $ "Class.intToWidth: no such width: " ++ show i

newtype GcFlag
  = MkGcFlag { getGcFlag :: Bool }
  deriving (Show, Eq, Ord)

newtype RegId
  = MkRegId { getRegId :: Int }
  deriving (Show, Eq, Ord)

data MachOp
  -- Arith
  = AAdd | ASub | AMul | ADiv | AMod | ANeg -- <- unary
  -- Rel
  | RLt | RLe | RGt | RGe | REq | RNe
  -- Logic
  | LAnd | LOr | LNot -- <- unary
  -- Bitwise
  | BAnd | BOr | BXor | BShr | BShl | BNot -- <- unary
  -- Memory
  | MRef StorageType
  deriving (Show, Eq, Ord)

showMachOp :: MachOp -> String
showMachOp o = case o of
  AAdd -> "+"
  ASub -> "-"
  AMul -> "*"
  ADiv -> "/"
  AMod -> "%"
  ANeg -> "-"
  RLt  -> "<"
  RLe  -> "<="
  RGt  -> ">"
  RGe  -> ">="
  REq  -> "=="
  RNe  -> "!="
  LAnd -> "&&"
  LOr  -> "||"
  LNot -> "!"
  BAnd -> "&"
  BOr  -> "|"
  BXor -> "^"
  BShr -> ">>"
  BShl -> "<<"
  BNot -> "~"
  MRef _ -> "[MRef]"

reverseCond :: MachOp -> MachOp
reverseCond op = case op of
  RLt -> RGe
  RLe -> RGt
  REq -> RNe
  RNe -> REq
  RGt -> RLe
  RGe -> RLt
  _ -> error $ "Backend.Class.reverseCond: not a cond: " ++ show op

isCondOp :: MachOp -> Bool
isCondOp op = case op of
  RLt -> True
  RLe -> True
  REq -> True
  RNe -> True
  RGt -> True
  RGe -> True
  _ -> False

isLogicOp :: MachOp -> Bool
isLogicOp op = case op of
  LOr -> True
  LAnd -> True
  LNot -> True
  _ -> False

-- Platform independent (huh?) GcMap impl
data GcMap
  = GcMap {
    gmLabel :: Imm, -- the label for return addr

    -- callee-saved register saved in the prolog
    gmPrologSavedRegs :: [(Reg, Int)],

    -- escaped callee-saved registers that contain gcptrs
    gmEscapedRegs :: [Reg],

    gmFramePtrOffset :: Int, -- relative to &retAddr?

    gmGcptrOffsets :: [Int] -- relative to rbp
  }

-- Commonly used
newRegV :: MonadUnique m => StorageType -> m Operand
newRegV (kls, width, gc) = do
  i <- mkUnique
  return . OpReg $ RegV (VirtualReg (MkRegId i, kls, width, gc))

newTempLabel :: MonadUnique m => String -> m Imm
newTempLabel s = do
  i <- mkUnique
  return . TempLabel s $ i

-- For regalloc

mkUseOfAddr (MkAddr mbBase mbIndex _)
  = toList mbBase ++ map fst (toList mbIndex)

mkUseOfSrc :: Operand -> [Reg]
mkUseOfSrc op = case op of
  OpReg r -> [r]
  OpImm _ -> []
  OpAddr addr -> mkUseOfAddr addr

mkUseOfDest :: Operand -> [Reg]
mkUseOfDest op = case op of
  OpReg _ -> []
  OpImm _ -> []
  OpAddr addr -> mkUseOfAddr addr

mkDefOfSrc :: Operand -> [Reg]
mkDefOfSrc _ = []

mkDefOfDest :: Operand -> [Reg]
mkDefOfDest op = case op of
  OpReg r -> [r]
  OpImm _ -> []
  OpAddr _ -> []

replaceRegInOp :: (Reg -> Reg) -> Operand -> Operand
replaceRegInOp f op = case op of
  OpReg r -> OpReg (f r)
  OpImm _ -> op
  OpAddr (MkAddr mbBase mbIndex disp) ->
    OpAddr (MkAddr (fmap f mbBase) (fmap (first f) mbIndex) disp)

gcFlagOfReg :: Reg -> GcFlag
gcFlagOfReg r = case r of
  RegP (PhysicalReg (rid, kls, _, gcf)) -> gcf
  RegV (VirtualReg (rid, kls, _, gcf)) -> gcf

copyGcFlag :: Reg -> Reg -> Reg
copyGcFlag from to = to'
  where
    gcf = case from of
            RegV (VirtualReg (_, _, _, gcf)) -> gcf
    to' = case to of
            RegP (PhysicalReg (rid, kls, w, _)) ->
              RegP (PhysicalReg (rid, kls, w, gcf))

copyOpWidth :: Reg -> Reg -> Reg
copyOpWidth from to = to'
  where
    w = case from of
          RegV (VirtualReg (_, _, w, _)) -> w
    to' = setOpWidth w to

setOpWidth :: OpWidth -> Reg -> Reg
setOpWidth w r = r'
  where
    r' = case r of
      RegP (PhysicalReg (rid, kls, _, gcf)) ->
        RegP (PhysicalReg (rid, kls, w, gcf))
      RegV (VirtualReg (rid, kls, _, gcf)) ->
        RegV (VirtualReg (rid, kls, w, gcf))

