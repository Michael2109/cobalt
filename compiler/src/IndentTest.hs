{-# LANGUAGE TupleSections #-}

module IndentTest where

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



expr :: Parser Expr
expr = f <$> sepBy1 expr' (symbol ";")
  where
    -- if there's only one expr return it without using ‘Seq’
    f l = if length l == 1 then head l else Seq l

expr' :: Parser Expr
expr' = try moduleParser
  <|> try functionParser
  <|> try assignArith

moduleParser :: Parser Expr
moduleParser = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      rword "module"
      name <- identifier
      return (L.IndentSome Nothing (return . (Module name)) expr')

valueParser :: Parser Expr
valueParser = L.indentBlock scn p
  where
    p = do
      valueName <- valueToken
      symbol "="
      return (L.IndentMany Nothing (return . (Module valueName)) expr')


functionParser :: Parser Expr
functionParser = L.indentBlock scn p
  where
    p = do
      name <- identifier
      symbol ":"
      argTypes <- some argumentType
      symbol "->"
      rType <- IndentTest.returnType
      nameDup <- L.lineFold scn $ \sp' ->
        (identifier) `sepBy1` try sp' <* scn
      args <- many argument
      symbol "="
      if(name == "main") then
          return (L.IndentMany Nothing (return . (MainFunction name argTypes args rType)) expr')
      else
          return (L.IndentMany Nothing (return . (Function name argTypes args rType)) expr')

valueToken :: Parser String
valueToken = lexeme (takeWhile1P Nothing f) <?> "list item"
  where
    f x = isAlphaNum x || x == '-'

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


parser :: Parser Expr
parser = expr'

parsePrint :: String -> IO()
parsePrint s = parseTest' parser s