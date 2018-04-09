{-|
Module      : BaseParser
Description : Contains parsing functions used in all other parsers.
Generally a lower level parser for words etc.
-}
module Parser.BaseParser where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Data.Scientific
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Text.Pretty.Simple (pShow)

import Parser.ParserType

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

rword :: String -> Parser String
rword word = do
    lexeme (string word *> notFollowedBy alphaNumChar)
    return word

rws :: [String] -- list of reserved words
rws =
    [ "val"
    , "var"
    , "object"
    , "class"
    , "public"
    , "protected"
    , "private"
    , "this"
    , "mutable"
    , "new"
    , "package"
    , "println"
    , "package"
    , "import"
    , "implements"
    , "extends"
    , "let"
    , "if"
    , "then"
    , "else"
    , "while"
    , "do"
    , "skip"
    , "True"
    , "False"
    , "not"
    , "and"
    , "or"
    , "super"
    ]

word :: Parser String
word = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> alphaNumChar <*> many alphaNumChar
    check x = if x `elem` rws
                  then fail $ "keyword " ++ show x ++ " cannot be an word"
                  else return x

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

floatParser :: Parser Double
floatParser = do
    try $ do
        let value = lexeme L.float
        symbol "f"
        value

doubleParser :: Parser Scientific
doubleParser = do
    try $ do
        let value = lexeme L.scientific
        value

integerParser :: Parser Integer
integerParser = lexeme L.decimal

longParser :: Parser Scientific
longParser = do
    try $ do
        let value = lexeme L.scientific
        symbol "l"
        value

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x = if x `elem` rws
                  then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                  else return x
