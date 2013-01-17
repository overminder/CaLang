{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}

module Frontend.AST (
  Name, Binding, StorageType,
  Program, ToplevelDef(..), Func(..), Data(..), Scope(..),
  Stmt(..), Expr(..), Lit(..), isLVal,
  gcptr, i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, storageTypes,

  -- Generic (?) traverse
  traverseExprM,
  traverseExpr,
  liftExprM,

  -- Pretty printing
  pprProgram,
  pprFunc,
  pprData,
  pprBinding,
) where

import Control.Monad hiding (mapM)
import Control.Monad.Identity hiding (mapM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable
import Prelude hiding (mapM)
import Text.PrettyPrint

import Backend.Operand
import Utils.Class

import Data.List (mapAccumL)

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
    fName    :: a,
    fArgs    :: [Binding a],
    fBody    :: Stmt a,
    fExportC :: Bool
  }
  deriving (Show, Functor)

data Stmt a
  = SAssign (Expr a) (Expr a)
  | SVarDecl [Binding a]
  | SIf (Expr a) (Stmt a) (Stmt a)
  | SWhile (Expr a) (Stmt a)
  | SBlock [Stmt a]
  | SReturn (Maybe (Expr a))
  | SContinue
  | SBreak
  | SJump (Expr a)
  | SExpr (Expr a)
  | SLabel a
  | SSwitch (Expr a) [a] (Maybe a) -- expr labels jumpTable 
  deriving (Show, Functor)

data Expr a
  = ELit (Lit a) -- A source language literal
  | EVar a -- A variable, whose information is described in tyVar a
  | EAsm Reg -- Source-level assembly register
  | EBinary MachOp (Expr a) (Expr a) -- arith/rel/bool
  | EUnary MachOp (Expr a) -- not/neg
  | ECall [CallingConv] (Expr a) [Expr a] -- func [args..]
  deriving (Show, Functor)

data Lit a
  = LInt Integer
  | LChr Char
  | LStr String
  | LFlo Double
  | LSym a -- Reference a object symbol
  | LArr [Lit a] -- Currently only integral types are supported (int, flo, sym)
  deriving (Show, Eq, Ord, Functor)

binaryOpNames :: Map.Map MachOp String
binaryOpNames = Map.fromList [
  (AAdd, "+"), (ASub, "-"), (AMul, "*"), (ADiv, "/"), (AMod, "%"),
  (RLt, "<"), (RLe, "<="), (RGt, ">"), (RGe, ">="), (REq, "=="), (RNe, "!="),
  (LAnd, "&&"), (LOr, "||"),
  (BAnd, "&"), (BOr, "|"), (BXor, "^"), (BShr, ">>"), (BShl, "<<")
  ]

data Data a
  = LiteralData (Binding a) (Lit a)
  deriving (Show, Functor)

data Scope
  = Export [Name] Bool -- names and whether it uses C's callingconv
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
pprFunc (Func name args body isC)
  = pprExportType isC <+> ppr name <>
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

pprExportType conv = case conv of
  True -> text . show $ "C"
  False -> empty

pprScope s = case s of
  Export names conv -> text "export" <+> pprExportType conv <+>
                       hcat (punctuate comma (map text names)) <> semi
  Import names -> text "import" <+>
                  hcat (punctuate comma (map text names)) <> semi
  GlobalReg name r -> text "register" <+> text name <+>
                      doubleQuotes (pprReg r) <> semi

pprStmt :: Ppr a => Stmt a -> Doc
pprStmt s = case s of
  SAssign e1 e2 -> pprExpr e1 <+> char '=' <+> pprExpr e2 <> semi
  SVarDecl bs -> hcat (map (\b -> pprBinding b <> semi) bs)
  SIf e s1 s2 -> text "if" <+> pprExpr e <+>
                 pprStmt s1 $$
                 text "else" <+> pprStmt s2
  SWhile e s -> text "while" <+> pprExpr e <+> pprStmt s
  SContinue -> text "continue" <> semi
  SBreak -> text "break" <> semi
  SBlock xs -> braces (nest 4 (vcat (map pprStmt xs)))
  SReturn me -> text "return" <+> (case me of
    Just e -> pprExpr e
    Nothing -> empty) <> semi
  SJump e -> text "jump" <+> pprExpr e <> semi
  SExpr e -> pprExpr e <> semi
  SLabel name -> ppr name <> colon
  SSwitch e branches tab ->
    text "switch" <+> pprExpr e <+>
    brackets (hcat (punctuate comma (map ppr branches))) <+>
    (case tab of
      Just tab' -> text "=>" <+> ppr tab'
      Nothing -> empty) <> semi

pprExpr :: Ppr a => Expr a -> Doc
pprExpr e = case e of
  ELit lit -> pprLit lit
  EVar name -> ppr name
  EAsm r -> text "asm" <> parens (pprReg r)
  EBinary op e1 e2 -> pprExpr e1 <+> pprBinaryOp op <+> pprExpr e2
  EUnary op e -> case op of
    ANeg -> char '-' <> parens (pprExpr e)
    LNot -> char '!' <> parens (pprExpr e)
    BNot -> char '~' <> parens (pprExpr e)
    MRef ty -> pprStorageType ty <> brackets (pprExpr e)
  ECall conv func args ->
    text "call" <+>
    pprCallingConvs conv <+>
    pprExpr func <>
    parens (hcat (punctuate comma (map pprExpr args)))

pprLit lit = case lit of
  LInt i -> integer i
  LChr c -> text (show c)
  LStr s -> text (show s)
  LFlo d -> double d
  LSym s -> ppr s
  LArr xs -> braces (hcat (punctuate comma (map pprLit xs)))

pprBinaryOp op = text (binaryOpNames Map.! op)

-- Monadic traverse
traverseExprM :: Monad m => (Expr a -> m (Expr a)) -> Stmt a -> m (Stmt a)
traverseExprM f s = case s of
  SAssign e1 e2 -> do
    e1' <- f e1
    e2' <- f e2
    return $ SAssign e1' e2'
  SVarDecl _ -> return s
  SIf e s1 s2 -> do
    e' <- f e
    s1' <- traverseExprM f s1
    s2' <- traverseExprM f s2
    return $ SIf e' s1' s2'
  SWhile e s -> do
    e' <- f e
    s' <- traverseExprM f s
    return $ SWhile e' s'
  SBlock xs -> do
    xs' <- mapM (traverseExprM f) xs
    return $ SBlock xs'
  SReturn mbE -> liftM SReturn (mapM f mbE)
  SJump e -> liftM SJump (f e)
  SExpr e -> liftM SExpr (f e)
  SLabel _ -> return s
  SSwitch e xs tab -> do
    e' <- f e
    return $ SSwitch e' xs tab
  SContinue -> return SContinue
  SBreak -> return SBreak

traverseExpr :: (Expr a -> Expr a) -> Stmt a -> Stmt a
traverseExpr f s = runIdentity (traverseExprM (return . f) s)

liftExprM :: Monad m => (Expr a -> m (Expr a, [Stmt a])) ->
                        Stmt a -> m (Stmt a)
liftExprM f s = do
  xs <- flatten_lift s
  return $ if length xs == 1 then head xs else SBlock xs
  where
  flatten_lift s = case s of
    SAssign e1 e2 -> do
      (e1', ss1) <- f e1
      (e2', ss2) <- f e2
      return $ ss1 ++ ss2 ++ [SAssign e1' e2']
    SVarDecl _ -> return [s]
    SIf e s1 s2 -> do
      (e', ss) <- f e
      s1' <- liftExprM f s1
      s2' <- liftExprM f s2
      return $ ss ++ [SIf e' s1' s2']
    SWhile e s -> do
      (e', ss) <- f e
      s' <- liftExprM f s
      return $ ss ++ [SWhile e' s']
    SBlock xs -> do
      liftM ((:[]) . SBlock . concat) (mapM flatten_lift xs)
    SReturn mbE -> case mbE of
      Just e -> do
        (e', ss) <- f e
        return $ ss ++ [SReturn (Just e')]
      Nothing -> return [s]
    SJump e -> do
      (e', ss) <- f e
      return $ ss ++ [SJump e']
    SExpr e -> do
      (e', ss) <- f e
      return $ ss ++ [SExpr e']
    SLabel _ -> return [s]
    SSwitch e labels tab -> do
      (e', ss) <- f e
      return $ ss ++ [SSwitch e' labels tab]
    SContinue -> return [SContinue]
    SBreak -> return [SBreak]

