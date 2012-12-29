module Frontend.Parser (
  readProgram
) where

import Control.Monad
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as T

import Frontend.AST
import Backend.Operand
import Backend.Class

languageDef
  = emptyDef { T.commentStart    = "/*"
             , T.commentEnd      = "*/"
             , T.nestedComments  = True
             , T.commentLine     = "//"
             , T.identStart      = letter <|> char '_'
             , T.identLetter     = alphaNum <|> char '_'
             , T.reservedNames   = [ "if"
                                   , "else"
                                   , "while"
                                   , "return"
                                   , "call"
                                   , "jump"
                                   , "switch"
                                   , "export"
                                   , "import"
                                   , "register"
                                   ] ++ (map fst storageTypes)
             , T.reservedOpNames = words ("+ - * / = < <= > >= " ++
                                          "== != && || ! % ~ & | ^ " ++
                                          "<< >> :")
             , T.caseSensitive   = True
             }

lexer = T.makeTokenParser languageDef

-- Token defs
ident = T.identifier lexer
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
parens = T.parens lexer
braces = T.braces lexer
brackets = T.brackets lexer
numLit = T.naturalOrFloat lexer
strLit = T.stringLiteral lexer
semi = T.semi lexer
comma = T.comma lexer
ws = T.whiteSpace lexer

-- Syntax defs
pProgram = ws >> many pToplevel

pToplevel = pFunction <|> pData <|> pScope

pFunction = do
  name <- ident
  args <- pArgDecls
  stmt <- pStmt
  return . FuncDef $ Func name args stmt

pData = do
  binding <- pTypeBinding
  reservedOp "="
  lit <- pLit
  semi
  return . DataDef $ LiteralData binding lit

pScope = pExport <|> pImport <|> pGlobalReg

pExport = do
  reserved "export"
  names <- sepBy ident comma
  semi
  return . ScopeDef $ Export names

pImport = do
  reserved "import"
  names <- sepBy ident comma
  semi
  return . ScopeDef $ Import names

pGlobalReg = do
  reserved "register"
  varName <- ident
  regName <- strLit
  semi
  case mkRegFromString regName of
    Just r -> return . ScopeDef $ GlobalReg varName r
    Nothing -> fail $ "No such register: " ++ regName

pArgDecls = parens (sepBy pTypeBinding comma)

pTypeBinding = do
  ty <- pStorageType
  name <- ident
  return (ty, name)

storageTypeMap :: Map.Map String StorageType
storageTypeMap = Map.fromList storageTypes

pStmtList = do
  xs <- many pStmt
  return $ SBlock xs

pStmt = braces pStmtList <|> pCompoundStmt

pCompoundStmt = pIfStmt
            <|> pWhileStmt
            <|> pReturnStmt
            <|> pJumpStmt
            <|> pSwitchStmt
            <|> try pLabelStmt
            <|> try pAssignStmt
            <|> pDeclStmt
            <|> pExprStmt

pIfStmt = do
  reserved "if"
  cond <- pExpr
  thenStmt <- pStmt
  elseOrPass <- pElse <|> pNothing
  return $ SIf cond thenStmt elseOrPass

pElse = do
  reserved "else"
  body <- pStmt
  return body

pNothing = return $ SBlock []

pWhileStmt = do
  reserved "while"
  cond <- pExpr
  body <- pStmt
  return $ SWhile cond body

pReturnStmt = do
  reserved "return"
  e <- (liftM Just (try pExpr)) <|> (return Nothing)
  semi
  return $ SReturn e

pJumpStmt = do
  reserved "jump"
  e <- pExpr
  semi
  return $ SJump e

pSwitchStmt = do
  reserved "switch"
  e <- pExpr
  branches <- brackets (sepBy ident comma)
  semi
  return $ SSwitch e branches

pLabelStmt = do
  name <- ident
  reservedOp ":"
  return $ SLabel name

pAssignStmt = do
  lVal <- pExpr
  guard $ isLVal lVal
  reservedOp "="
  rVal <- pExpr
  semi
  return $ SAssign lVal rVal

pDeclStmt = do
  binding <- pTypeBinding
  semi
  return $ SVarDecl binding

pExprStmt = do
  e <- pExpr
  semi
  guard $ is_call e
  return $ SExpr e
  where
    is_call (ECall _ _ _) = True
    is_call _ = False

pExpr = buildExpressionParser opList (pCall <|> pTerm)

opList = -- Unary
         [ [Prefix (reservedOp "~"  >> return (EUnary  BNot))]
         , [Prefix (reservedOp "!"  >> return (EUnary  LNot))]
         , [Prefix (reservedOp "-"  >> return (EUnary  ANeg))]
         -- Bitwise and/or
         , [Infix  (reservedOp "&"  >> return (EBinary BAnd))  AssocLeft]
         , [Infix  (reservedOp "|"  >> return (EBinary BOr))   AssocLeft]
         , [Infix  (reservedOp "^"  >> return (EBinary BXor))  AssocLeft]
         -- Arith
         , [Infix  (reservedOp "%"  >> return (EBinary AMod))  AssocLeft]
         , [Infix ((reservedOp "*"  >> return (EBinary AMul))  <|>
                   (reservedOp "/"  >> return (EBinary ADiv))) AssocLeft]
         , [Infix ((reservedOp "+"  >> return (EBinary AAdd))  <|>
                   (reservedOp "-"  >> return (EBinary ASub))) AssocLeft]
         -- Bitwise shift
         , [Infix ((reservedOp "<<" >> return (EBinary BShl))  <|>
                   (reservedOp ">>" >> return (EBinary BShr))) AssocLeft]
         -- Relational
         , [Infix  (reservedOp "<"  >> return (EBinary RLt ))  AssocLeft]
         , [Infix  (reservedOp "<=" >> return (EBinary RLe ))  AssocLeft]
         , [Infix  (reservedOp ">"  >> return (EBinary RGt ))  AssocLeft]
         , [Infix  (reservedOp ">=" >> return (EBinary RGe ))  AssocLeft]
         , [Infix  (reservedOp "==" >> return (EBinary REq ))  AssocLeft]
         , [Infix  (reservedOp "!=" >> return (EBinary RNe ))  AssocLeft]
         -- Logical
         , [Infix  (reservedOp "&&" >> return (EBinary LAnd))  AssocLeft]
         , [Infix  (reservedOp "||" >> return (EBinary LOr ))  AssocLeft]
         ]

pCall = do
  reserved "call"
  maybeConv <- (try strLit <|> return "")
  func <- pTerm
  args <- pArgs
  case parseCallingConv maybeConv of
    Just conv -> return $ ECall conv func args
    Nothing -> fail $ "And there's no such calling convention: " ++ maybeConv

pArgs = parens $ sepBy pExpr comma

pTerm = parens pExpr
    <|> pMemDeref
    <|> liftM ELit pLit
    <|> liftM EVar ident

pLit = liftM mk_num numLit
   <|> liftM LStr strLit
   where
     mk_num num = case num of
       Left i -> LInt i
       Right d -> LFlo d

pMemDeref = do
  mcls <- pStorageType
  e <- brackets pExpr
  return $ EUnary (MRef mcls) e

pStorageType = foldr1 (<|>) (map p_type storageTypes)
  where
    p_type (s, ty) = do
      reserved s
      return ty

readProgram :: String -> Program String
readProgram str = case parse pProgram "<Ca source>" str of
  Left e -> error $ show e
  Right r -> r
