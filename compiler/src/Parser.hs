{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Data.Char (isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

import Block

type Parser = Parsec Void String

-- Tokens


lineComment :: Parser ()
lineComment = L.skipLineComment "#"


scn :: Parser ()
scn = L.space space1 lineComment empty


sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment empty
  where
    f x = x == ' ' || x == '\t'


symbol :: String -> Parser String
symbol = L.symbol sc


rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)


rws :: [String] -- list of reserved words
rws = ["module", "println", "import",  "let", "if","then","else","while","do","skip","true","false","not","and","or"]


word :: Parser String
word = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> alphaNumChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an word"
                else return x

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


integer :: Parser Integer
integer = lexeme L.decimal


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


aTerm :: Parser AExpr
aTerm = parens aExpr
  <|> Var      <$> identifier
  <|> IntConst <$> integer


aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/") ]
  , [ InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-") ]
  ]


aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators


assignArith :: Parser Expr
assignArith = do
  var  <- identifier
  symbol ":"
  vType <- valType
  symbol "="
  e <- aExpr
  return $ AssignArith vType var e


bTerm :: Parser BExpr
bTerm =  parens bExpr
  <|> (BoolConst True  <$ rword "true")
  <|> (BoolConst False <$ rword "false")
  <|> rExpr


bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [Prefix (Not <$ rword "not") ]
  , [InfixL (BBinary And <$ rword "and")
    , InfixL (BBinary Or <$ rword "or") ]
  ]


bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators


rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return (RBinary op a1 a2)


relation :: Parser RBinOp
relation = (symbol ">" *> pure Greater)
  <|> (symbol "<" *> pure Less)


identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x


stringLiteral :: Parser Expr
stringLiteral = do
  value <- char '"' >> manyTill L.charLiteral (char '"')
  return $ StringLiteral value


assignString :: Parser Expr
assignString = do
  var  <- identifier
  symbol ":"
  vType <- valType
  symbol "="
  e <- stringLiteral
  return (AssignString vType var e)


arrayDef :: Parser Expr
arrayDef = do
  name <- identifier
  symbol ":"

  symbol "["
  arrType <- word
  symbol "]"

  symbol "="
  return $ ArrayDef arrType name

arrayValues :: Parser Expr
arrayValues = do
  symbol "["
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


moduleParser :: Parser Expr
moduleParser = do
    rword "module"
    name <- identifier
    --es <- many expr'
    imports <- many (try importParser)
    exprs <- many $ (expr' name <|> functionParser name)
    return (Module name imports exprs)


importParser :: Parser Expr
importParser = L.nonIndented scn p
  where
    p = do
      rword "import"
      locations <- sepBy1 identifier (symbol ".")
      return $ (Import locations)

-- Function parser
functionParser :: String -> Parser Expr
functionParser moduleName = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      name <- identifier
      symbol ":"
      argTypes <- some argumentType
      symbol "->"
      rType <- Parser.returnType
      nameDup <- L.lineFold scn $ \sp' -> identifier
      args <- many argument
      symbol "="
      if(name == "main") then
          return (L.IndentMany Nothing (return . (MainFunction moduleName name argTypes args rType)) (expr' moduleName))
      else
          return (L.IndentMany Nothing (return . (Function moduleName name argTypes args rType)) (expr' ""))

valType :: Parser Expr
valType = do
    value <- identifier
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


functionCallParser :: String -> Parser Expr
functionCallParser moduleName = do
  name <- identifier
  args <- parens $ many argument
  return $ FunctionCall moduleName name args


-- data A = B String | C Integer
dataElementParser :: String -> Parser Expr
dataElementParser superName = do
  name <- identifier
  argTypes <- many identifier
  let args = map (\x -> "var" ++ show x) [0..((length argTypes)-1)]
  return $ DataElement superName name argTypes args


dataParser :: Parser Expr
dataParser = L.nonIndented scn p
  where
    p = do
      rword "data"
      name <- identifier
      symbol "="
      dataElements <- sepBy (dataElementParser name) (symbol "|")
      return $ Data name dataElements


dataInstanceParser :: String -> Parser Expr
dataInstanceParser moduleName = do
  typeName <- valType
  es <- expr' moduleName
  return $ DataInstance moduleName typeName es


printParser :: Parser Expr
printParser = do
  rword "println"
  bodyArr <- identifier
  return $ Print bodyArr


valueToken :: Parser String
valueToken = lexeme (takeWhile1P Nothing f) <?> "list item"
  where
    f x = isAlphaNum x || x == '-'


ifStmt :: String -> Parser Expr
ifStmt moduleName = L.indentBlock scn p
   where
     p = do
       rword "if"
       cond  <- bExpr
       return (L.IndentMany Nothing (return . (If cond)) (expr' moduleName))

elseIfStmt :: String -> Parser Expr
elseIfStmt moduleName = L.indentBlock scn p
   where
     p = do
       rword "else"
       rword "if"
       cond  <- bExpr
       return (L.IndentMany Nothing (return . (ElseIf cond)) (expr' moduleName))

elseStmt :: String -> Parser Expr
elseStmt moduleName = L.indentBlock scn p
   where
     p = do
       rword "else"
       return (L.IndentMany Nothing (return . (Else)) (expr' moduleName))

whereStmt :: Parser Expr
whereStmt = do
  rword "where"
  symbol "{"
  exprs <- many expr
  symbol "}"
  return $ (Where exprs)


expr :: Parser Expr
expr = f <$> sepBy1 (expr' "") (symbol ";")
  where
    -- if there's only one expr return it without using ‘Seq’
    f l = if length l == 1 then head l else Seq l


expr' :: String -> Parser Expr
expr' moduleName = try moduleParser
  <|> try dataParser
  <|> try (functionCallParser moduleName)
  <|> try (ifStmt moduleName)
  <|> try (elseIfStmt moduleName)
  <|> try (elseStmt moduleName)
  <|> try arrayAssign
  <|> try arrayElementSelect
  <|> try assignArith
  <|> try assignString
  <|> try printParser
  <|> try (dataInstanceParser moduleName)
  <|> try whereStmt
  <|> try stringLiteral


parser :: Parser Expr
parser = expr' ""


parseFromFile file = runParser expr file <$> readFile file


parseString input =
  case parse (expr' "") "" input of
    Left  e -> show e
    Right x -> show x


parsePrint :: String -> IO()
parsePrint s = parseTest' parser s