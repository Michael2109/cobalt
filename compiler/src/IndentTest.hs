{-# LANGUAGE TupleSections #-}

module IndentTest where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Data.Char (isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Block

type Parser = Parsec Void String

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

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x


pItemList :: Parser (String, [(String, [String])])
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentSome Nothing (return . (header, )) pComplexItem)

pComplexItem :: Parser (String, [String])
pComplexItem = L.indentBlock scn p
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pLineFold)

pItem :: Parser String
pItem = lexeme (takeWhile1P Nothing f) <?> "list item"
  where
    f x = isAlphaNum x || x == '-'


pLineFold :: Parser String
pLineFold = L.lineFold scn $ \sc' ->
  let ps = takeWhile1P Nothing f `sepBy1` try sc'
      f x = isAlphaNum x || x == '-'
  in unwords <$> ps <* sc

parser :: Parser (String, [(String, [String])])
parser = pItemList <* eof

expr :: Parser Expr
expr = f <$> sepBy1 expr' (symbol ";")
  where
    -- if there's only one expr return it without using ‘Seq’
    f l = if length l == 1 then head l else Seq l

expr' :: Parser Expr
expr' = try moduleParser
  <|> try functionParser
  <|> valueParser

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


exprParser :: Parser Expr
exprParser = expr'

parsePrint :: String -> IO()
parsePrint s = parseTest' exprParser s