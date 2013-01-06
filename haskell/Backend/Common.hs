module Backend.Common (
  StorageType,
  Imm(..),
  OpClass(..),
  OpWidth(..), opWidths, intToWidth,
  GcFlag(..),
  RegId(..),
  MachOp(..), reverseCond, isCondOp, showMachOp,
) where

import Utils.Unique

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
