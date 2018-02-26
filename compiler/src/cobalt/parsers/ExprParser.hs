{-|
Module      : ExprParser
Description : Parses all expressions.
The highest level parser that uses functions in the BaseParser and ABExprParser to generate the AST.
-}
module ExprParser (Parser,
  expr, objectParser, classParser, parser) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Data.Char (isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Text.Pretty.Simple (pShow)

import ABExprParser
import Block
import SymbolTable


objectParser :: ClassSymbolTable -> [String] -> Parser Expr
objectParser symbolTable relativeDir = try $ L.nonIndented scn p
  where
    p = do
      imports <- many (try (importParser symbolTable))
      moduleT <- L.lineFold scn $ \sp' -> try (rword "object")
      name <- identifier
      exprs <- many (functionParser  symbolTable name True <|> expr' symbolTable)
      let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
      return (Module (packageDir) name imports exprs)

classParser :: ClassSymbolTable -> [String] -> Parser Expr
classParser symbolTable relativeDir = try $ L.nonIndented scn p
  where
    p = do
      imports <- many (try (importParser symbolTable))

      classT <- L.lineFold scn $ \sp' -> try (rword "class")
      name <- identifier
      params <- optional (parens (sepBy (parameterParser symbolTable) (symbol ",")))
      extendsKeyword <- optional (rword "extends")
      parent <- optional (identifier)
      implementsKeyword <- optional (rword "implements")
      interfaces <- optional (identifier)
      modifierBlocks <- many (try (modifierBlockParser symbolTable))
      --let constructorExprs = []
      constructorExprs <- try (many $ try (constructorExpr symbolTable))
      exprs <- many (functionParser symbolTable name False <|> expr' symbolTable)
      let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
      return (Class (packageDir) name params parent interfaces imports modifierBlocks constructorExprs exprs)


importParser ::  ClassSymbolTable -> Parser Expr
importParser symbolTable = try $ L.nonIndented scn p
  where
    p = do
      try (rword "import")
      locations <- sepBy1 identifier (symbol ".")
      return $ (Import locations)

-- For the class parameters
parameterParser ::  ClassSymbolTable -> Parser Expr
parameterParser symbolTable = do
  varName  <- identifierParser symbolTable
  symbol ":"
  varType <- identifierParser symbolTable
  return $ ClassParam varType varName

-- Constructor exprs
constructorExpr ::  ClassSymbolTable -> Parser Expr
constructorExpr symbolTable = try $ L.nonIndented scn p
  where
    p = do
      e <- expr' symbolTable
      return e



-- Function parser
functionParser ::  ClassSymbolTable -> String -> Bool -> Parser Expr
functionParser symbolTable moduleName static = try $ L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      annotations <- try (optional (annotationParser symbolTable))
      name <- L.lineFold scn $ \sp' -> identifier
      symbol ":"
      aList <- sepBy (identifierParser symbolTable <|> arrayType symbolTable) (symbol "->")
      let argTypes = take (length aList - 1) aList
      let rType = last aList
      nameDup <- L.lineFold scn $ \sp' -> identifier
      args <- many (argument symbolTable)
      symbol "="
      if(name == "main")
        then return (L.IndentMany Nothing (return . (MainFunction name annotations argTypes args rType)) (expr' symbolTable))
        else if name == moduleName
          then return (L.IndentMany Nothing (return . (Constructor name argTypes args)) (expr' symbolTable))
          else return (L.IndentMany Nothing (return . (Function name annotations argTypes args rType static)) (expr' symbolTable))


identifierParser ::  ClassSymbolTable -> Parser Expr
identifierParser symbolTable = do
  name <- identifier
  return $ Identifier name

arithmeticParser ::  ClassSymbolTable -> Parser Expr
arithmeticParser symbolTable = do
  aE <- aExpr
  return $ ArithExpr aE

booleanParser ::  ClassSymbolTable -> Parser Expr
booleanParser symbolTable = do
  bE <- try bExpr
  return $ BooleanExpr bE

stringLiteral ::  ClassSymbolTable -> Parser Expr
stringLiteral symbolTable = do
  value <- char '"' >> manyTill L.charLiteral (char '"')
  return $ StringLiteral value


assignParser :: ClassSymbolTable ->Parser Expr
assignParser symbolTable = do
  try (rword "val" <|> rword "var")
  varName <- identifierParser symbolTable
  symbol ":"
  varType <- valType symbolTable
  symbol "="
  e <- expr' symbolTable <|> arithmeticParser symbolTable <|> identifierParser symbolTable
  return $ Assign varType varName e


reassignParser :: ClassSymbolTable ->Parser Expr
reassignParser symbolTable = do
  name  <- try $ do
    id <- identifier
    symbol "="
    return id

  value <- expr' symbolTable <|> arithmeticParser symbolTable <|> identifierParser symbolTable
  return (Reassign name value)

arrayType :: ClassSymbolTable ->Parser Expr
arrayType symbolTable = do
  symbol "["
  arrType <- identifier
  symbol "]"
  return $ ArrayType arrType

arrayDef :: ClassSymbolTable ->Parser Expr
arrayDef symbolTable = do
  name <-
    try $ do
      id <- identifier
      symbol ":"
      symbol "["
      return id
  arrType <- word
  symbol "]"
  symbol "="
  return $ ArrayDef arrType name


arrayValues :: ClassSymbolTable -> Parser Expr
arrayValues symbolTable = do
  try (symbol "[")
  values <- many identifier
  symbol "]"
  return $ ArrayValues values

arrayAssign :: ClassSymbolTable -> Parser Expr
arrayAssign symbolTable = do
  def <- arrayDef symbolTable
  values <- arrayValues symbolTable
  return $ ArrayAssignment def values

arrayElementSelect :: ClassSymbolTable -> Parser Expr
arrayElementSelect symbolTable = do
  symbol "!!"
  elementNum <- word
  return $ ArrayElementSelect elementNum

arrayAppend :: ClassSymbolTable -> Parser Expr
arrayAppend symbolTable = do
  arrays <- sepBy1 (expr' symbolTable) (symbol "++")
  return $ ArrayAppend arrays

thisMethodCall :: ClassSymbolTable -> Parser Expr
thisMethodCall symbolTable  = do
  methodName <- identifier
  args <- parens (sepBy (expr' symbolTable <|> argument symbolTable <|> identifierParser symbolTable <|> arithmeticParser symbolTable) (symbol ","))
  return $ ThisMethodCall methodName args


objectMethodCall :: ClassSymbolTable -> Parser Expr
objectMethodCall symbolTable = do
  objectName <- identifier
  symbol "."
  methodName <- identifier
  args <- parens (sepBy (expr' symbolTable <|> argument symbolTable <|> identifierParser symbolTable <|> arithmeticParser symbolTable) (symbol ","))
  if(objectName == "super")
    then return $ SuperMethodCall objectName methodName args
    else return $ ObjectMethodCall objectName methodName args


newClassInstance :: ClassSymbolTable -> Parser Expr
newClassInstance symbolTable  = do
  try (rword "new")
  className <- identifier
  arguments <- parens (sepBy (expr' symbolTable  <|> argument symbolTable) (symbol ","))
  return $ (NewClassInstance className arguments)

classVariable :: ClassSymbolTable -> Parser Expr
classVariable symbolTable  = do
  className <- identifier
  symbol "."
  varName <- identifier
  return $ ClassVariable className varName

modifierBlockParser :: ClassSymbolTable -> Parser Expr
modifierBlockParser symbolTable  = try $ L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      modifier <- try (rword "public") <|> try (rword "protected") <|> try (rword "private")
      return (L.IndentMany Nothing (return . (ModifierBlock)) (globalVarParser symbolTable modifier))


globalVarParser :: ClassSymbolTable -> String -> Parser Expr
globalVarParser symbolTable  modifier = do
  final <- try (rword "val" <|> rword "var")
  varName <- identifierParser symbolTable
  symbol ":"
  varType <- valType symbolTable
  symbol "="
  es <- many (expr' symbolTable <|> arithmeticParser symbolTable <|> identifierParser symbolTable)
  return $ GlobalVar modifier (final == "val") varType varName es

annotationParser :: ClassSymbolTable -> Parser Expr
annotationParser symbolTable  = do
  try (symbol "@")
  name <- identifier
  return $ Annotation name


valType :: ClassSymbolTable -> Parser Expr
valType symbolTable = do
    value <- identifierParser symbolTable <|> arrayType symbolTable
    return $ Type value

argumentType :: ClassSymbolTable -> Parser Expr
argumentType symbolTable = do
    value <- identifier
    return $ ArgumentType value

returnType :: ClassSymbolTable -> Parser Expr
returnType symbolTable = do
    value <- identifier
    return $ ReturnType value

argument :: ClassSymbolTable -> Parser Expr
argument symbolTable = do
  value <- identifier
  return $ Argument value


functionCallParser :: ClassSymbolTable -> Parser Expr
functionCallParser symbolTable  = do
  name <- identifier
  args <- parens $ many (argument symbolTable)
  return $ FunctionCall  name args


-- data A = B String | C Integer
dataElementParser :: ClassSymbolTable -> String -> Parser Expr
dataElementParser symbolTable superName = do
  name <- identifier
  argTypes <- many identifier
  let args = map (\x -> "var" ++ show x) [0..((length argTypes)-1)]
  return $ DataElement superName name argTypes args

lambdaParser :: ClassSymbolTable -> Parser Expr
lambdaParser symbolTable  = do
  try (symbol "\\")
  varName <- identifier
  symbol "->"
  es <- some (expr' symbolTable)
  return $ Lambda varName es

dataParser :: ClassSymbolTable -> Parser Expr
dataParser symbolTable = L.nonIndented scn p
  where
    p = do
      rword "data"
      name <- identifier
      symbol "="
      dataElements <- sepBy (dataElementParser symbolTable name) (symbol "|")
      return $ Data name dataElements


dataInstanceParser :: ClassSymbolTable -> Parser Expr
dataInstanceParser symbolTable  = do
  typeName <- valType symbolTable
  es <- parens (sepBy (expr' symbolTable <|> argument symbolTable) (symbol ","))
  return $ DataInstance typeName es



printParser :: ClassSymbolTable -> Parser Expr
printParser symbolTable  = do
  rword "println"
  e <- expr'  symbolTable
  return $ Print e


ifStmt :: ClassSymbolTable ->Parser Expr
ifStmt symbolTable  = L.indentBlock scn p
   where
     p = do
       try (rword "if")
       cond  <- bExpr
       return (L.IndentMany Nothing (return . (If cond)) (expr' symbolTable))

elseIfStmt :: ClassSymbolTable -> Parser Expr
elseIfStmt symbolTable  = L.indentBlock scn p
   where
     p = do
       try $ do
         rword "else"
         rword "if"
       cond  <- bExpr
       return (L.IndentMany Nothing (return . (ElseIf cond)) (expr'  symbolTable))

elseStmt :: ClassSymbolTable -> Parser Expr
elseStmt symbolTable  = L.indentBlock scn p
   where
     p = do
       rword "else"
       return (L.IndentMany Nothing (return . (Else)) (expr'  symbolTable))

whileParser :: ClassSymbolTable -> Parser Expr
whileParser symbolTable  = L.indentBlock scn p
   where
     p = do
       try (rword "while")
       e <- parens (booleanParser symbolTable)
       return (L.IndentMany Nothing (return . (While e)) (expr'  symbolTable))

tryParser :: ClassSymbolTable -> Parser Expr
tryParser symbolTable  = L.indentBlock scn p
   where
     p = do
       rword "try"
       return (L.IndentMany Nothing (return . (Try)) (expr' symbolTable))


catchParser :: ClassSymbolTable -> Parser Expr
catchParser symbolTable  = L.indentBlock scn p
   where
     p = do
       rword "catch"
       params <- optional (parens (sepBy (parameterParser symbolTable) (symbol ",")))
       let argType = "Exception"
       let argName = "e"
       return (L.IndentMany Nothing (return . (Catch params)) (expr' symbolTable))

whereStmt :: ClassSymbolTable -> Parser Expr
whereStmt symbolTable = do
  rword "where"
  symbol "{"
  exprs <- many (expr symbolTable)
  symbol "}"
  return $ (Where exprs)

thisParser :: ClassSymbolTable -> Parser Expr
thisParser symbolTable = do
  try (rword "this")
  return This

superParser :: ClassSymbolTable -> Parser Expr
superParser symbolTable = do
  rword "super"
  return Super

expr :: ClassSymbolTable -> Parser Expr
expr symbolTable = f <$> sepBy1 (expr' symbolTable) (symbol ";")
  where
    -- if there's only one expr return it without using ‘Seq’
    f l = if length l == 1 then head l else Seq l


expr' :: ClassSymbolTable -> Parser Expr
expr' symbolTable = try (dataParser symbolTable)
  <|> try (functionCallParser symbolTable)
  <|> booleanParser symbolTable

  -- If else
  <|> try (ifStmt symbolTable)
  <|> try (elseIfStmt symbolTable)
  <|> try (elseStmt symbolTable)

  <|> try (whileParser symbolTable)


  -- try/catch
  <|> try (tryParser symbolTable)
  <|> try (catchParser symbolTable)
  <|> try (printParser symbolTable)
  <|> try (objectMethodCall symbolTable)
  <|> try (thisMethodCall symbolTable)
  <|> newClassInstance symbolTable
  <|> try (classVariable symbolTable)
  <|> try (dataInstanceParser symbolTable)
  <|> arrayAssign symbolTable
  <|> try (arrayElementSelect symbolTable)
  <|> lambdaParser symbolTable

  <|> assignParser symbolTable
  <|> reassignParser symbolTable

  <|> thisParser symbolTable

  <|> try (whereStmt symbolTable)
  <|> try (stringLiteral symbolTable)


parser :: Parser Expr
parser = expr' $ ClassSymbolTable "ClassName" [] []