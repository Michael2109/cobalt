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
                    arithmeticParser,
                    assignParser,
                    booleanParser,
                    classVariableParser,
                    elseIfStmtParser,
                    elseStmtParser,
                    identifierParser,
                    ifStmtParser,
                    importParser,
                    forLoopParser,
                    methodCallParser,
                    methodParser,
                    modifierBlockParser,
                    newClassInstanceParser,
                    objectMethodCallParser,
                    packageParser,
                    parameterizedTypeParser,
                    parameterParser,
                    reassignParser,
                    superMethodCallParser,
                    stringLiteralParser,
                    stringLiteralMultilineParser,
                    thisMethodCallParser,
                    thisVarParser,
                    typeParameterParser,
                    valueTypeParser
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
  <|> newClassInstanceParser
  <|> objectMethodCallParser
  <|> methodCallParser
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
  <|> newClassInstanceParser
  <|> objectMethodCallParser
  <|> methodCallParser
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
      typeParam <- optional typeParameterParser
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
      exprs <- many (methodParser name False <|> expr')
      let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
      return (Object (packageDir) name typeParam params parent interfaces imports modifierBlocks constructorExprs exprs)

classParser :: [String] -> Parser Expr
classParser relativeDir = try $ L.nonIndented scn p
  where
    p = do
      imports <- many importParser
      classT <- L.lineFold scn $ \sp' -> try (rword "class")
      name <- identifier
      typeParam <- optional typeParameterParser
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
      exprs <- many (methodParser name False <|> expr')
      let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
      return (Class (packageDir) name typeParam params parent interfaces imports modifierBlocks constructorExprs exprs)

traitParser :: [String] -> Parser Expr
traitParser relativeDir = try $ L.nonIndented scn p
  where
    p = do
      imports <- many importParser
      classT <- L.lineFold scn $ \sp' -> try (rword "trait")
      name <- identifier
      typeParam <- optional typeParameterParser
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
      exprs <- many (methodParser name False <|> expr')
      let packageDir = if (length relativeDir <= 1) then [] else (tail relativeDir)
      return (Trait (packageDir) name typeParam params parent interfaces imports modifierBlocks constructorExprs exprs)

packageParser :: Parser Expr
packageParser = try $ L.nonIndented scn p
  where
    p = do
      try (rword "package")
      locations <- sepBy1 identifier (symbol ".")
      return $ (Package locations)

importParser :: Parser Expr
importParser = try $ L.nonIndented scn p
  where
    p = do
      try (rword "import")
      locations <- sepBy1 identifier (symbol ".")
      return $ (Import locations)

typeParameterParser :: Parser Expr
typeParameterParser = do
  try $ symbol "["
  typeName <- identifierParser
  symbol "]"
  return $ TypeParameter typeName

parameterParser :: Parser Expr
parameterParser = do
  varName  <- identifierParser
  symbol ":"
  varType <- identifierParser
  return $ Parameter varType varName

constructorExpr :: Parser Expr
constructorExpr = try $ L.nonIndented scn p
  where
    p = do
      e <- expr'
      return e

methodParser :: String -> Bool -> Parser Expr
methodParser moduleName static = try $ L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      annotations <- try (optional annotationParser)
      name <- identifierParser
      fullParams <- optional (parens (sepBy parameterParser (symbol ",")))
      let params = case fullParams of
                     Just ps -> ps
                     Nothing -> []
      symbol ":"
      rType <- identifierParser
      return (L.IndentMany Nothing (return . (Function name annotations params rType static)) (expr'))

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
  valVar <- try (rword "val" <|> rword "var")
  let immutable = valVar == "val"

  varName <- identifierParser
  varType <- optional $ do
    symbol ":"
    vType <- valueTypeParser
    return vType
  symbol "="
  e <- expr' <|> identifierParser <|> arithmeticParser
  return $ Assign immutable varType varName e


reassignParser :: Parser Expr
reassignParser = do
  name  <- try $ do
    id <- identifierParser <|> thisVarParser
    symbol "="
    return id

  value <- expr' <|>  identifierParser <|> arithmeticParser <|> booleanParser
  return (Reassign name value)

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

methodCallParser :: Parser Expr
methodCallParser =
  try $ do
    methodName <- identifier
    args <- parens (sepBy (argumentParser) (symbol ","))
    return $ MethodCall methodName args

objectMethodCallParser :: Parser Expr
objectMethodCallParser =
  try $ do
    objectName <- identifier
    symbol "."
    methodName <- identifier
    args <- parens (sepBy (argumentParser) (symbol ","))
    return $ ObjectMethodCall objectName methodName args


newClassInstanceParser :: Parser Expr
newClassInstanceParser  = do
  try (rword "new")
  className <- identifierParser
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
  varType <- valueTypeParser
  symbol "="
  es <- many (argumentParser)
  return $ GlobalVar modifier (final == "val") static varType varName es

annotationParser :: Parser Expr
annotationParser  = do
  try (symbol "@")
  name <- identifier
  return $ Annotation name


valueTypeParser :: Parser Expr
valueTypeParser = do
    value <- parameterizedTypeParser <|> identifierParser
    return $ Type value

parameterizedTypeParser :: Parser Expr
parameterizedTypeParser = do
  try $ do
    className <- identifierParser
    typeParameter <- typeParameterParser
    return $ ParameterizedType className typeParameter

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
  value <- thisParser <|> classVariableParser <|> booleanParser <|> newClassInstanceParser <|> stringLiteralParser <|> stringLiteralMultilineParser <|> identifierParser <|> arithmeticParser
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
expr' =

  forLoopParser
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
  <|> newClassInstanceParser
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
