{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}

module Frontend.AST (
  Name, Binding, StorageType,
  Program, ToplevelDef(..), Func(..), Data(..), Scope(..),
  Stmt(..), Expr(..), Lit(..), BinaryOp(..), UnaryOp(..), isLVal,
  gcptr, i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, storageTypes,

  -- Pretty printing
  pprProgram,
) where

import Text.PrettyPrint
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.Operand
import Utils.Class

type Program a = [ToplevelDef a]
type Name = String

instance Ppr Name where
  ppr = text

type Binding a = (StorageType, a)

data ToplevelDef a
  = FuncDef (Func a)
  | DataDef (Data a)
  | ScopeDef Scope
  deriving (Show, Functor)

data Func a
  = Func {
    fName :: a,
    fArgs :: [Binding a],
    fBody :: Stmt a
  }
  deriving (Show, Functor)

data Stmt a
  = SAssign (Expr a) (Expr a)
  | SVarDecl (Binding a)
  | SIf (Expr a) (Stmt a) (Stmt a)
  | SWhile (Expr a) (Stmt a)
  | SBlock [Stmt a]
  | SReturn (Maybe (Expr a))
  | SJump (Expr a)
  | SExpr (Expr a)
  | SLabel a
  | SSwitch (Expr a) [a]
  deriving (Show, Functor)

data Expr a
  = ELit Lit -- A source language literal
  | EVar a -- A variable, whose information is described in tyVar a
  | EBinary BinaryOp (Expr a) (Expr a) -- arith/rel/bool
  | EUnary UnaryOp (Expr a) -- not/neg
  | ECall [CallingConv] (Expr a) [Expr a] -- func [args..]
  deriving (Show, Functor)

data Lit
  = LInt Integer
  | LStr String
  | LFlo Double
  deriving (Show, Eq, Ord)

data BinaryOp
  -- Arith
  = AAdd | ASub | AMul | ADiv | AMod
  -- Rel
  | RLt | RLe | RGt | RGe | REq | RNe
  -- Logic
  | LAnd | LOr
  -- Bitwise
  | BAnd | BOr | BXor | BShr | BShl
  deriving (Show, Eq, Ord)

binaryOpNames :: Map.Map BinaryOp String
binaryOpNames = Map.fromList [
  (AAdd, "+"), (ASub, "-"), (AMul, "*"), (ADiv, "/"), (AMod, "%"),
  (RLt, "<"), (RLe, "<="), (RGt, ">"), (RGe, ">="), (REq, "=="), (RNe, "!="),
  (LAnd, "&&"), (LOr, "||"),
  (BAnd, "&"), (BOr, "|"), (BXor, "^"), (BShr, ">>"), (BShl, "<<")
  ]

data UnaryOp
  = ANeg -- Arith neg
  | LNot -- Logical not
  | BNot -- Bitwise not
  | MRef StorageType -- Mem
  deriving (Show, Eq, Ord)

data Data a
  = LiteralData (Binding a) Lit
  deriving (Show, Functor)

data Scope
  = Export [Name]
  | Import [Name]
  | GlobalReg Name Reg
  deriving (Show)

-- Supplementary types and functions

-- XXX: actually only one kind of EVar can be lval...
isLVal :: Expr a -> Bool
isLVal e = case e of
  EVar _ -> True
  EUnary (MRef _) _ -> True
  _ -> False

type StorageType = (OpClass, OpWidth, GcFlag)

gcptr :: StorageType
gcptr = (SignedOp, W64, MkGcFlag True)

u8, u16, u32, u64, i8, i16, i32, i64, f32, f64 :: StorageType
[u8, u16, u32, u64] = map (\w -> (UnsignedOp, w, MkGcFlag False)) opWidths
[i8, i16, i32, i64] = map (\w -> (SignedOp, w, MkGcFlag False)) opWidths
f32 = (FloatingOp, W32, MkGcFlag False)
f64 = (FloatingOp, W64, MkGcFlag False)

storageTypes :: [(String, StorageType)]
storageTypes = zip (words "gcptr i8 i16 i32 i64 u8 u16 u32 u64 f32 f64")
                   [gcptr, i8, i16, i32, i64, u8, u16, u32, u64, f32, f64]

-- Pretty printing
pprProgram :: Ppr a => Program a -> Doc
pprProgram = vcat . map pprToplevel

pprToplevel :: Ppr a => ToplevelDef a -> Doc
pprToplevel t = case t of
  FuncDef f -> pprFunc f
  DataDef d -> pprData d
  ScopeDef s -> pprScope s

pprFunc :: Ppr a => Func a -> Doc
pprFunc (Func name args body)
  = ppr name <>
    parens (hcat (punctuate comma (map pprBinding args))) <+>
    pprStmt body

pprBinding :: Ppr a => Binding a -> Doc
pprBinding (ty, name) = pprStorageType ty <+> ppr name

pprStorageType ty = case ty of
  (FloatingOp, w, _) -> text "f" <> pprWidth w
  (SignedOp, _, (MkGcFlag True)) -> text "gcptr"
  (SignedOp, w, _) -> text "i" <> pprWidth w
  (UnsignedOp, w, _) -> text "u" <> pprWidth w

pprData (LiteralData b lit) = pprBinding b <+> char '=' <+>
                              pprLit lit <> semi

pprScope s = case s of
  Export names -> text "export" <+>
                  hcat (punctuate comma (map text names)) <> semi
  Import names -> text "import" <+>
                  hcat (punctuate comma (map text names)) <> semi
  GlobalReg name r -> text "register" <+> text name <+>
                      doubleQuotes (pprReg r) <> semi

pprStmt :: Ppr a => Stmt a -> Doc
pprStmt s = case s of
  SAssign e1 e2 -> pprExpr e1 <+> char '=' <+> pprExpr e2 <> semi
  SVarDecl b -> pprBinding b <> semi
  SIf e s1 s2 -> text "if" <+> pprExpr e <+>
                 pprStmt s1 $$
                 text "else" <+> pprStmt s2
  SWhile e s -> text "while" <+> pprExpr e <+> pprStmt s
  SBlock xs -> braces (nest 4 (vcat (map pprStmt xs)))
  SReturn me -> text "return" <+> (case me of
    Just e -> pprExpr e
    Nothing -> empty) <> semi
  SJump e -> text "jump" <+> pprExpr e <> semi
  SExpr e -> pprExpr e <> semi
  SLabel name -> ppr name <> semi
  SSwitch e branches ->
    text "switch" <+> pprExpr e <+>
    brackets (hcat (punctuate comma (map ppr branches))) <> semi

pprExpr :: Ppr a => Expr a -> Doc
pprExpr e = case e of
  ELit lit -> pprLit lit
  EVar name -> ppr name
  EBinary op e1 e2 -> pprExpr e1 <+> pprBinaryOp op <+> pprExpr e2
  EUnary op e -> case op of
    ANeg -> char '-' <> pprExpr e
    LNot -> char '!' <> pprExpr e
    BNot -> char '~' <> pprExpr e
    MRef ty -> pprStorageType ty <> brackets (pprExpr e)
  ECall conv func args ->
    text "call" <+>
    pprCallingConvs conv <+>
    pprExpr func <>
    parens (hcat (punctuate comma (map pprExpr args)))

pprLit lit = case lit of
  LInt i -> integer i
  LStr s -> doubleQuotes (zeroWidthText s)
  LFlo d -> double d

pprBinaryOp op = text (binaryOpNames Map.! op)

