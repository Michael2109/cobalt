{-|
Module      : ExprParser
Description : Parses all expressions.
The highest level parser that uses functions in the BaseParser and ABExprParser to generate the AST.
-}
module ExprParser (Parser,
                    expr,
                    objectParser,
                    classParser,
                    traitParser,
                    parser,
                    annotationParser,
                    booleanParser,
                    argumentParser
                    ) where

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
      classT <- L.lineFold scn $ \sp' -> try (rword "object")
      name <- identifier
      params <- optional (parens (sepBy parameterParser (symbol ",")))
      extendsKeyword <- optional (rword "extends")
      parent <- optional (identifier)
      implementsKeyword <- optional (rword "implements")
      interfaces <- optional (identifier)
      modifierBlocks <- many (try (modifierBlockParser True))
      constructorExprs <- try (many $ try constructorExpr)
      exprs <- many (functionParser name True <|> expr')
      let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
      return (Object (packageDir) name params parent interfaces imports modifierBlocks constructorExprs exprs)

classParser :: [String] -> Parser Expr
classParser relativeDir = try $ L.nonIndented scn p
  where
    p = do
      imports <- many (try importParser)
      classT <- L.lineFold scn $ \sp' -> try (rword "class")
      name <- identifier
      params <- optional (parens (sepBy parameterParser (symbol ",")))
      extendsKeyword <- optional (rword "extends")
      parent <- optional (identifier)
      implementsKeyword <- optional (rword "implements")
      interfaces <- optional (identifier)
      modifierBlocks <- many (try (modifierBlockParser False))
      constructorExprs <- try (many $ try constructorExpr)
      exprs <- many (functionParser name False <|> expr')
      let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
      return (Class (packageDir) name params parent interfaces imports modifierBlocks constructorExprs exprs)

traitParser :: [String] -> Parser Expr
traitParser relativeDir = try $ L.nonIndented scn p
  where
    p = do
      imports <- many (try importParser)
      classT <- L.lineFold scn $ \sp' -> try (rword "trait")
      name <- identifier
      params <- optional (parens (sepBy parameterParser (symbol ",")))
      extendsKeyword <- optional (rword "extends")
      parent <- optional (identifier)
      implementsKeyword <- optional (rword "implements")
      interfaces <- optional (identifier)
      modifierBlocks <- many (try (modifierBlockParser False))
      constructorExprs <- try (many $ try constructorExpr)
      exprs <- many (functionParser name False <|> expr')
      let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
      return (Class (packageDir) name params parent interfaces imports modifierBlocks constructorExprs exprs)

importParser :: Parser Expr
importParser = try $ L.nonIndented scn p
  where
    p = do
      try (rword "import")
      locations <- sepBy1 identifier (symbol ".")
      return $ (Import locations)

-- For the class parameters
parameterParser :: Parser Expr
parameterParser = do
  varName  <- identifierParser
  symbol ":"
  varType <- identifierParser
  return $ ClassParam varType varName

-- Constructor exprs
constructorExpr :: Parser Expr
constructorExpr = try $ L.nonIndented scn p
  where
    p = do
      e <- expr'
      return e



-- Function parser
functionParser :: String -> Bool -> Parser Expr
functionParser moduleName static = try $ L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      annotations <- try (optional annotationParser)
      name <- L.lineFold scn $ \sp' -> identifier
      symbol ":"
      aList <- sepBy (identifierParser  <|> arrayType) (symbol "->")
      let argTypes = take (length aList - 1) aList
      let rType = last aList
      nameDup <- L.lineFold scn $ \sp' -> identifier
      args <- many argumentParser
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

thisVarParser :: Parser Expr
thisVarParser = do
  try $ do
    rword "this"
    symbol "."
  name <- identifierParser
  return $ ThisVar name

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
assignParser = do
  try (rword "val" <|> rword "var")
  varName <- identifierParser
  symbol ":"
  varType <- valType
  symbol "="
  e <- expr' <|> arithmeticParser <|> identifierParser
  return $ Assign varType varName e


reassignParser :: Parser Expr
reassignParser = do
  name  <- try $ do
    id <- identifierParser <|> thisVarParser
    symbol "="
    return id

  value <- expr' <|> arithmeticParser <|> identifierParser
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
  args <- parens (sepBy (expr' <|> argumentParser <|> identifierParser <|> arithmeticParser) (symbol ","))
  return $ ThisMethodCall methodName args


objectMethodCall :: Parser Expr
objectMethodCall = do
  objectName <- identifier
  symbol "."
  methodName <- identifier
  args <- parens (sepBy (expr' <|> argumentParser <|> identifierParser <|> arithmeticParser) (symbol ","))
  if(objectName == "super")
    then return $ SuperMethodCall objectName methodName args
    else return $ ObjectMethodCall objectName methodName args


newClassInstance :: Parser Expr
newClassInstance  = do
  try (rword "new")
  className <- identifier
  arguments <- parens (sepBy (expr'  <|> argumentParser) (symbol ","))
  return $ (NewClassInstance className arguments)

classVariable ::Parser Expr
classVariable  = do
  className <- identifier
  symbol "."
  varName <- identifier
  return $ ClassVariable className varName

modifierBlockParser :: Bool -> Parser Expr
modifierBlockParser static = try $ L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      modifier <- try (rword "public") <|> try (rword "protected") <|> try (rword "private")
      return (L.IndentMany Nothing (return . (ModifierBlock)) (globalVarParser  modifier static))


globalVarParser :: String -> Bool -> Parser Expr
globalVarParser modifier static = do
  final <- try (rword "val" <|> rword "var")
  varName <- identifierParser
  symbol ":"
  varType <- valType
  symbol "="
  es <- many (expr' <|> arithmeticParser <|> identifierParser)
  return $ GlobalVar modifier (final == "val") static varType varName es

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

argumentParser :: Parser Expr
argumentParser = do
  value <- booleanParser <|> identifierParser
  return $ Argument value


functionCallParser :: Parser Expr
functionCallParser  = do
  name <- identifier
  args <- parens $ sepBy argumentParser (symbol ",")
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
  es <- parens (sepBy (expr' <|> argumentParser) (symbol ","))
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
       return (L.IndentMany Nothing (return . (If cond)) (expr' <|> arithmeticParser <|> booleanParser))

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
       params <- optional (parens (sepBy parameterParser (symbol ",")))
       let argType = "Exception"
       let argName = "e"
       return (L.IndentMany Nothing (return . (Catch params)) (expr' ))

whereStmt :: Parser Expr
whereStmt = do
  rword "where"
  symbol "{"
  exprs <- many expr
  symbol "}"
  return $ (Where exprs)

thisParser :: Parser Expr
thisParser = do
  try (rword "this")
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
  <|> reassignParser


  <|> thisVarParser
  <|> thisParser

  <|> try whereStmt
  <|> try stringLiteral


parser :: Parser Expr
parser = expr'