{-|
Module      : ExprParser
Description : Parses all expressions.
The highest level parser that uses functions in the BaseParser and ABExprParser to generate the AST.
-}
module ExprParser (Parser,
                    expr,
                    aExpr,
                    bExpr,
                    rExpr,
                    objectParser,
                    classParser,
                    traitParser,
                    parser,
                    annotationParser,
                    argumentParser,
                    argumentTypeParser,
                    booleanParser,
                    classVariableParser,
                    elseIfStmtParser,
                    elseStmtParser,
                    identifierParser,
                    ifStmtParser,
                    importParser,
                    forLoopParser,
                    modifierBlockParser,
                    objectMethodCallParser,
                    superMethodCallParser,
                    stringLiteralParser,
                    stringLiteralMultilineParser,
                    thisMethodCallParser,
                    thisVarParser
                    ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Text.Pretty.Simple (pShow)


import Block
import BaseParser
import SymbolTable

-- Arithmetic Expression Parsers
--
--

aTerm :: Parser Expr
aTerm = parens aExpr
  <|> classVariableParser
  <|> identifierParser
  <|> IntConst <$> integerParser
  <|> DoubleConst <$> doubleParser


aOperators :: [[Operator Parser Expr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/") ]
  , [ InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-") ]
  ]


aExpr :: Parser Expr
aExpr = makeExprParser aTerm aOperators


-- Boolean expression parsers
--
--

bTerm :: Parser Expr
bTerm = BoolConst True  <$ rword "True"
  <|> BoolConst False <$ rword "False"
  <|> parens bExpr
  <|> try rExpr
  <|> classVariableParser
  <|> identifierParser



bOperators :: [[Operator Parser Expr]]
bOperators =
  [ [Prefix (Not <$ rword "not") ]
  , [InfixL (BBinary And <$ rword "and")
    , InfixL (BBinary Or <$ rword "or") ]
  ]


bExpr :: Parser Expr
bExpr = makeExprParser bTerm bOperators


rExpr :: Parser Expr
rExpr = do
  try $ do
    a1 <- aExpr
    op <- relation
    a2 <- aExpr
    return (RBinary op a1 a2)


relation :: Parser Expr
relation = (symbol ">=" *> pure GreaterEqual)
  <|> (symbol "<=" *> pure LessEqual)
  <|> (symbol ">" *> pure Greater)
  <|> (symbol "<" *> pure Less)


objectParser :: [String] -> Parser Expr
objectParser relativeDir = try $ L.nonIndented scn p
  where
    p = do
      imports <- many importParser
      classT <- L.lineFold scn $ \sp' -> try (rword "object")
      name <- identifier
      fullParams <- optional (parens (sepBy parameterParser (symbol ",")))
      let params = case fullParams of
                  Just ps -> ps
                  Nothing -> []
      extendsKeyword <- optional (rword "extends")
      parent <- optional (identifier)
      implementsKeyword <- optional (rword "implements")
      interfaces <- sepBy identifier (symbol ",")
      modifierBlocks <- many (try (modifierBlockParser True))
      constructorExprs <- try (many $ try constructorExpr)
      exprs <- many (functionParser name True <|> expr')
      let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
      return (Object (packageDir) name params parent interfaces imports modifierBlocks constructorExprs exprs)

classParser :: [String] -> Parser Expr
classParser relativeDir = try $ L.nonIndented scn p
  where
    p = do
      imports <- many importParser
      classT <- L.lineFold scn $ \sp' -> try (rword "class")
      name <- identifier
      fullParams <- optional (parens (sepBy parameterParser (symbol ",")))
      let params = case fullParams of
                  Just ps -> ps
                  Nothing -> []
      extendsKeyword <- optional (rword "extends")
      parent <- optional (identifier)
      implementsKeyword <- optional (rword "implements")
      interfaces <- sepBy identifier (symbol ",")
      modifierBlocks <- many (try (modifierBlockParser False))
      constructorExprs <- try (many $ try constructorExpr)
      exprs <- many (functionParser name False <|> expr')
      let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
      return (Class (packageDir) name params parent interfaces imports modifierBlocks constructorExprs exprs)

traitParser :: [String] -> Parser Expr
traitParser relativeDir = try $ L.nonIndented scn p
  where
    p = do
      imports <- many importParser
      classT <- L.lineFold scn $ \sp' -> try (rword "trait")
      name <- identifier
      fullParams <- optional (parens (sepBy parameterParser (symbol ",")))
      let params = case fullParams of
                  Just ps -> ps
                  Nothing -> []
      extendsKeyword <- optional (rword "extends")
      parent <- optional (identifier)
      implementsKeyword <- optional (rword "implements")
      interfaces <- sepBy identifier (symbol ",")
      modifierBlocks <- many (try (modifierBlockParser False))
      constructorExprs <- try (many $ try constructorExpr)
      exprs <- many (functionParser name False <|> expr')
      let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
      return (Trait (packageDir) name params parent interfaces imports modifierBlocks constructorExprs exprs)



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
      aList <- sepBy (argumentTypeParser <|> identifierParser  <|> arrayType) (symbol "->")
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

stringLiteralParser :: Parser Expr
stringLiteralParser = do
  value <- char '"' >> manyTill L.charLiteral (char '"')
  return $ StringLiteral value

stringLiteralMultilineParser :: Parser Expr
stringLiteralMultilineParser = do
  symbol "```"
  contents <- many (L.lineFold scn $ \sp' -> some L.charLiteral)
  symbol "```"
  return $ StringLiteral $ intercalate "\n" contents


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

arrayValues :: Parser Expr
arrayValues = do
  try (symbol "[")
  values <- many identifier
  symbol "]"
  return $ ArrayValues values

arrayElementSelect :: Parser Expr
arrayElementSelect = do
  symbol "!!"
  elementNum <- word
  return $ ArrayElementSelect elementNum

arrayAppend :: Parser Expr
arrayAppend = do
  arrays <- sepBy1 (expr') (symbol "++")
  return $ ArrayAppend arrays

thisMethodCallParser :: Parser Expr
thisMethodCallParser =
  try $ do
    rword "this"
    symbol "."
    methodName <- identifier
    args <- parens (sepBy (argumentParser) (symbol ","))
    return $ ThisMethodCall methodName args

superMethodCallParser :: Parser Expr
superMethodCallParser =
  try $ do
    rword "super"
    symbol "."
    methodName <- identifier
    args <- parens (sepBy (argumentParser) (symbol ","))
    return $ SuperMethodCall methodName args

objectMethodCallParser :: Parser Expr
objectMethodCallParser =
  try $ do
    objectName <- identifier
    symbol "."
    methodName <- identifier
    args <- parens (sepBy (argumentParser) (symbol ","))
    return $ ObjectMethodCall objectName methodName args


newClassInstance :: Parser Expr
newClassInstance  = do
  try (rword "new")
  className <- identifier
  arguments <- parens (sepBy (argumentParser) (symbol ","))
  return $ (NewClassInstance className arguments)

classVariableParser ::Parser Expr
classVariableParser  =
  try $ do
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
  es <- many (argumentParser)
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

argumentTypeParser :: Parser Expr
argumentTypeParser = do
    value <- identifier
    return $ ArgumentType value

returnType :: Parser Expr
returnType = do
    value <- identifier
    return $ ReturnType value

argumentParser :: Parser Expr
argumentParser = do
  value <- thisParser <|> classVariableParser <|> booleanParser <|> newClassInstance <|> stringLiteralParser <|> stringLiteralMultilineParser <|> identifierParser <|> arithmeticParser
  return $ Argument value

printParser :: Parser Expr
printParser  = do
  try $ rword "println"
  e <- parens argumentParser
  return $ Print e

ifStmtParser :: Parser Expr
ifStmtParser  = try $ L.indentBlock scn p
   where
     p = do
       rword "if"
       cond  <- parens argumentParser
       return (L.IndentMany Nothing (return . (If cond)) (expr' <|> argumentParser))

elseIfStmtParser :: Parser Expr
elseIfStmtParser  = try $ L.indentBlock scn p
   where
     p = do
       try $ do
         rword "else"
         rword "if"
       cond  <- parens argumentParser
       return (L.IndentMany Nothing (return . (ElseIf cond)) (expr' <|> argumentParser))

elseStmtParser :: Parser Expr
elseStmtParser  = try $ L.indentBlock scn p
   where
     p = do
       try $ rword "else"
       return (L.IndentMany Nothing (return . (Else)) (expr' <|> argumentParser))

whileParser :: Parser Expr
whileParser  = try $ L.indentBlock scn p
   where
     p = do
       rword "while"
       e <- parens (booleanParser )
       return (L.IndentMany Nothing (return . (While e)) (expr' ))

forLoopParser :: Parser Expr
forLoopParser  = try $ L.indentBlock scn p
   where
     p = do
       rword "for"
       symbol "("
       varName <- identifier
       symbol "<-"
       start <- arithmeticParser
       rword "to"
       end <- arithmeticParser
       symbol ")"
       return (L.IndentMany Nothing (return . (For varName start end)) (expr' ))

tryParser :: Parser Expr
tryParser  = try $ L.indentBlock scn p
   where
     p = do
       rword "try"
       return (L.IndentMany Nothing (return . (Try)) (expr' ))


catchParser :: Parser Expr
catchParser  = try $ L.indentBlock scn p
   where
     p = do
       rword "catch"
       fullParams <- optional (parens (sepBy parameterParser (symbol ",")))
       let params = case fullParams of
                      Just ps -> ps
                      Nothing -> []
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
expr' = booleanParser

  <|> forLoopParser
  <|> ifStmtParser
  <|> elseIfStmtParser
  <|> elseStmtParser

  <|> whileParser


  <|> tryParser
  <|> catchParser
  <|> printParser
  <|> thisMethodCallParser
  <|> superMethodCallParser
  <|> objectMethodCallParser
  <|> newClassInstance
  <|> classVariableParser
  <|> try arrayElementSelect

  <|> assignParser
  <|> reassignParser


  <|> thisVarParser
  <|> thisParser

  <|> try whereStmt
  <|> try stringLiteralParser
  <|> try stringLiteralMultilineParser


parser :: Parser Expr
parser = expr'
