module Parser where

import           Data.Functor                  ((<$>), (<$))
import           Control.Applicative           (Applicative(..))
import qualified Control.Monad                 as M
import Control.Monad (void)
import           Data.Functor.Identity
import           Data.Text                     (Text)
import qualified Data.Text                     as Text

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Perm
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Text.Pretty.Simple
import Data.Either.Unwrap

--import Lexer
import Block

type Parser = Parsec Void String

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space (void $ takeWhile1P Nothing f) lineComment empty
  where
    f x = x == ' ' || x == '\t' || x =='\n'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

semi :: Parser String
semi = symbol ";"

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["module", "println", "import",  "let", "if","then","else","while","do","skip","true","false","not","and","or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

word :: Parser String
word = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> alphaNumChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an word"
                else return x

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

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

whileParser :: Parser Expr
whileParser = between sc eof expr

expr :: Parser Expr
expr = f <$> sepBy1 expr' semi
  where
    -- if there's only one expr return it without using ‘Seq’
    f l = if length l == 1 then head l else Seq l

expr' :: Parser Expr
expr' =
  try moduleParser
  <|> try importParser
  <|> try function
  <|> try functionCall
  <|> printParser
  <|> ifStmt
  <|> whileStmt
  <|> whereStmt
  <|> skipStmt
  <|> try arrayAssign
  <|> arrayElementSelect
  <|> try assignArith
  <|> try assignString
  <|> parens expr
  <|> stringLiteral

ifStmt :: Parser Expr
ifStmt = do
    rword "if"
    cond  <- bExpr
    symbol "{"
    expr1 <- many expr
    symbol "}"
    rword "else"
    symbol "{"
    expr2 <- many expr
    symbol "}"
    return (If cond expr1 expr2)

whileStmt :: Parser Expr
whileStmt = do
  rword "while"
  cond <- bExpr
  symbol "{"
  exprs <- some expr
  symbol "}"
  return (While cond exprs)

whereStmt :: Parser Expr
whereStmt = do
  rword "where"
  symbol "{"
  exprs <- many expr
  symbol "}"
  return $ (Where exprs)


moduleParser :: Parser Expr
moduleParser = do
  rword "module"
  name <- identifier
  symbol "{"
  body <- many expr
  symbol "}"
  return $ Module name body

importParser :: Parser Expr
importParser = do
  rword "import"
  firstDir <- word
  --symbol "."
  --directory <- some ((symbol ".") word)
  return $ Import (firstDir) ""

function :: Parser Expr
function = do
  name <- identifier
  symbol ":"
  argTypes <- some argumentType
  symbol "->"
  rType <- Parser.returnType
  nameDup <- identifier
  args <- many argument
  symbol "="
  symbol "{"
  bodyArr <- many expr
  symbol "}"
  if(name == "main") then
      return $ MainFunction name argTypes args rType bodyArr
  else
      return $ Function name argTypes args rType bodyArr

functionCall :: Parser Expr
functionCall = do
  name <- identifier
  args <- parens $ many argument
  return $ FunctionCall name args

printParser :: Parser Expr
printParser = do
  rword "println"
  bodyArr <- identifier
  symbol ";"
  return $ Print bodyArr

assignArith :: Parser Expr
assignArith = do
  var  <- identifier
  symbol ":"
  vType <- valType
  symbol "="
  e <- aExpr
  return (AssignArith vType var e)

assignString :: Parser Expr
assignString = do
  var  <- identifier
  symbol ":"
  vType <- valType
  symbol "="
  e <- stringLiteral
  return (AssignString vType var e)

skipStmt :: Parser Expr
skipStmt = Skip <$ rword "skip"

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-") ]

  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/") ]
  , [ InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-") ]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [Prefix (Not <$ rword "not") ]
  , [InfixL (BBinary And <$ rword "and")
    , InfixL (BBinary Or <$ rword "or") ]
  ]


aTerm :: Parser AExpr
aTerm = parens aExpr
  <|> Var      <$> identifier
  <|> IntConst <$> integer

stringLiteral :: Parser Expr
stringLiteral = do
  value <- char '"' >> manyTill L.charLiteral (char '"')
  symbol ";"
  return $ StringLiteral value

bTerm :: Parser BExpr
bTerm =  parens bExpr
  <|> (BoolConst True  <$ rword "true")
  <|> (BoolConst False <$ rword "false")
  <|> rExpr

rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return (RBinary op a1 a2)

relation :: Parser RBinOp
relation = (symbol ">" *> pure Greater)
  <|> (symbol "<" *> pure Less)

parsePrint :: String -> IO()
parsePrint s = do
    parseTest' expr' s

parseFromFile file = runParser expr file <$> readFile file

parseString input =
  case parse expr' "" input of
    Left  e -> show e
    Right x -> show x

--prettyPrint :: (Either (ParseError (Expr) e) a) -> IO()
--prettyPrint p = do
  --  pPrint p