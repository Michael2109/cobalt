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


objectParser :: [String] -> Parser Expr
objectParser relativeDir = try $ L.nonIndented scn p
  where
    p = do
      imports <- many (try importParser)
      moduleT <- L.lineFold scn $ \sp' -> try (rword "object")
      name <- identifier
      exprs <- many (functionParser name True <|> expr')
      let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
      return (Module (packageDir) name imports exprs)

classParser :: [String] -> Parser Expr
classParser relativeDir = try $ L.nonIndented scn p
  where
    p = do
      imports <- many (try importParser)

      classT <- L.lineFold scn $ \sp' -> try (rword "class")
      name <- identifier
      extendsKeyword <- optional (rword "extends")
      parent <- optional (identifier)
      implementsKeyword <- optional (rword "implements")
      interfaces <- optional (identifier)
      modifierBlocks <- many (modifierBlockParser)
      exprs <- many (functionParser name False <|> expr')
      let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
      return (Class (packageDir) name parent interfaces imports modifierBlocks exprs)


importParser :: Parser Expr
importParser = try $ L.nonIndented scn p
  where
    p = do
      try (rword "import")
      locations <- sepBy1 identifier (symbol ".")
      return $ (Import locations)

-- Function parser
functionParser :: String -> Bool -> Parser Expr
functionParser moduleName static = try $ L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      annotations <- try (optional (L.lineFold scn $ \sp' -> annotationParser ))
      name <- identifier
      symbol ":"
      aList <- sepBy (identifierParser  <|> arrayType) (symbol "->")
      let argTypes = take (length aList - 1) aList
      let rType = last aList
      nameDup <- L.lineFold scn $ \sp' -> identifier
      args <- many argument
      symbol "="
      if(name == "main")
        then return (L.IndentMany Nothing (return . (MainFunction name annotations argTypes args rType)) (expr'))
        else if name == moduleName
          then return (L.IndentMany Nothing (return . (Constructor name argTypes args)) (expr'))
          else return (L.IndentMany Nothing (return . (Function name annotations argTypes args rType static)) (expr'))


identifierParser :: Parser Expr
identifierParser = do
  name <- identifier
  return $ Identifier name

arithmeticParser :: Parser Expr
arithmeticParser = do
  aE <- aExpr
  return $ ArithExpr aE

booleanParser :: Parser Expr
booleanParser = do
  bE <- try bExpr
  return $ BooleanExpr bE

stringLiteral :: Parser Expr
stringLiteral = do
  value <- char '"' >> manyTill L.charLiteral (char '"')
  return $ StringLiteral value


assignParser :: Parser Expr
assignParser  = do
  varName <-
    try $ do
      varN  <- identifierParser
      symbol ":"
      return varN
  varType <-
    try $ do
      vType <- valType
      symbol "="
      return vType
  e <- expr'
  return $ Assign varType varName e


reassignParser :: Parser Expr
reassignParser = do
  name  <- identifier
  symbol "="
  value <- expr'
  return (Reassign name value)

arrayType :: Parser Expr
arrayType = do
  symbol "["
  arrType <- identifier
  symbol "]"
  return $ ArrayType arrType

arrayDef :: Parser Expr
arrayDef = do
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


arrayValues :: Parser Expr
arrayValues = do
  try (symbol "[")
  values <- many identifier
  symbol "]"
  return $ ArrayValues values

arrayAssign :: Parser Expr
arrayAssign = do
  def <- arrayDef
  values <- arrayValues
  return $ ArrayAssignment def values

arrayElementSelect :: Parser Expr
arrayElementSelect = do
  symbol "!!"
  elementNum <- word
  return $ ArrayElementSelect elementNum

arrayAppend :: Parser Expr
arrayAppend = do
  arrays <- sepBy1 (expr') (symbol "++")
  return $ ArrayAppend arrays

thisMethodCall :: Parser Expr
thisMethodCall  = do
  methodName <- identifier
  args <- parens (sepBy (expr' <|> argument) (symbol ","))
  return $ ThisMethodCall methodName args


objectMethodCall :: Parser Expr
objectMethodCall = do
  objectName <- identifier
  symbol "."
  methodName <- identifier
  args <- parens (sepBy (expr' <|> argument) (symbol ","))
  if(objectName == "super")
    then return $ SuperMethodCall objectName methodName args
    else return $ ObjectMethodCall objectName methodName args


newClassInstance :: Parser Expr
newClassInstance  = do
  try (rword "new")
  className <- identifier
  arguments <- parens (sepBy (expr'  <|> argument) (symbol ","))
  return $ (NewClassInstance className arguments)

classVariable ::Parser Expr
classVariable  = do
  className <- identifier
  symbol "."
  varName <- identifier
  return $ ClassVariable className varName

modifierBlockParser :: Parser Expr
modifierBlockParser  = try $ L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      modifier <- try (rword "public") <|> try (rword "protected") <|> try (rword "private")
      return (L.IndentMany Nothing (return . (ModifierBlock)) (globalVarParser  modifier))


globalVarParser :: String -> Parser Expr
globalVarParser  modifier = do
  varName <- identifierParser
  symbol ":"
  varType <- valType
  symbol "="
  es <- many (expr')
  return $ GlobalVar modifier varType varName es

annotationParser :: Parser Expr
annotationParser  = do
  try (symbol "@")
  name <- identifier
  return $ Annotation name


valType :: Parser Expr
valType = do
    value <- identifierParser <|> arrayType
    return $ Type value

argumentType :: Parser Expr
argumentType = do
    value <- identifier
    return $ ArgumentType value

returnType :: Parser Expr
returnType = do
    value <- identifier
    return $ ReturnType value

argument :: Parser Expr
argument = do
  value <- identifier
  return $ Argument value


functionCallParser :: Parser Expr
functionCallParser  = do
  name <- identifier
  args <- parens $ many argument
  return $ FunctionCall  name args


-- data A = B String | C Integer
dataElementParser :: String -> Parser Expr
dataElementParser superName = do
  name <- identifier
  argTypes <- many identifier
  let args = map (\x -> "var" ++ show x) [0..((length argTypes)-1)]
  return $ DataElement superName name argTypes args

lambdaParser :: Parser Expr
lambdaParser  = do
  try (symbol "\\")
  varName <- identifier
  symbol "->"
  es <- some (expr' )
  return $ Lambda varName es

dataParser :: Parser Expr
dataParser = L.nonIndented scn p
  where
    p = do
      rword "data"
      name <- identifier
      symbol "="
      dataElements <- sepBy (dataElementParser name) (symbol "|")
      return $ Data name dataElements


dataInstanceParser :: Parser Expr
dataInstanceParser  = do
  typeName <- valType
  es <- parens (sepBy (expr' <|> argument) (symbol ","))
  return $ DataInstance typeName es



printParser :: Parser Expr
printParser  = do
  rword "println"
  e <- expr'
  return $ Print e


ifStmt :: Parser Expr
ifStmt  = L.indentBlock scn p
   where
     p = do
       try (rword "if")
       cond  <- bExpr
       return (L.IndentMany Nothing (return . (If cond)) (expr' ))

elseIfStmt :: Parser Expr
elseIfStmt  = L.indentBlock scn p
   where
     p = do
       try $ do
         rword "else"
         rword "if"
       cond  <- bExpr
       return (L.IndentMany Nothing (return . (ElseIf cond)) (expr' ))

elseStmt :: Parser Expr
elseStmt  = L.indentBlock scn p
   where
     p = do
       rword "else"
       return (L.IndentMany Nothing (return . (Else)) (expr' ))

whileParser :: Parser Expr
whileParser  = L.indentBlock scn p
   where
     p = do
       try (rword "while")
       e <- parens (booleanParser )
       return (L.IndentMany Nothing (return . (While e)) (expr' ))

tryParser :: Parser Expr
tryParser  = L.indentBlock scn p
   where
     p = do
       rword "try"
       return (L.IndentMany Nothing (return . (Try)) (expr' ))


catchParser :: Parser Expr
catchParser  = L.indentBlock scn p
   where
     p = do
       rword "catch"
       let argType = "Exception"
       let argName = "e"
       return (L.IndentMany Nothing (return . (Catch argType argName)) (expr' ))

whereStmt :: Parser Expr
whereStmt = do
  rword "where"
  symbol "{"
  exprs <- many expr
  symbol "}"
  return $ (Where exprs)

thisParser :: Parser Expr
thisParser = do
  rword "this"
  return This

superParser :: Parser Expr
superParser = do
  rword "super"
  return Super

expr :: Parser Expr
expr = f <$> sepBy1 (expr') (symbol ";")
  where
    -- if there's only one expr return it without using ‘Seq’
    f l = if length l == 1 then head l else Seq l


expr' :: Parser Expr
expr' = try dataParser
  <|> try (functionCallParser )
  <|> booleanParser

  -- If else
  <|> try (ifStmt )
  <|> try (elseIfStmt )
  <|> try (elseStmt )

  <|> try (whileParser )


  -- try/catch
  <|> try (tryParser)
  <|> try (catchParser)
  <|> try (printParser)
  <|> try (objectMethodCall)
  <|> try (thisMethodCall)
  <|> newClassInstance
  <|> try (classVariable)
  <|> try (dataInstanceParser)
  <|> arrayAssign
  <|> try arrayElementSelect
  <|> lambdaParser

  <|> assignParser
  <|> try (reassignParser)

  <|> try (thisParser)

  <|> try (arithmeticParser)
  <|> try whereStmt
  <|> try stringLiteral
  <|> try (identifierParser)


parser :: Parser Expr
parser = expr'