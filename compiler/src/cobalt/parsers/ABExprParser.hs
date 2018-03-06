{-|
Module      : ABExprParser
Description : Parses arithmetic and boolean expressions.
-}
module ABExprParser (Parser,
                      parens, symbol, rword, rws, scn, identifier, word,
                      aTerm, aExpr, bExpr) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Data.Char (isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Text.Pretty.Simple (pShow)

import BaseParser
import ABBlock
import SymbolTable

-- Arithmetic Expression Parsers
--
--

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


-- Boolean expression parsers
--
--

bTerm :: Parser BExpr
bTerm = BoolConst True  <$ rword "True"
  <|> BoolConst False <$ rword "False"
  <|> parens bExpr
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
  <|> (symbol ">=" *> pure GreaterEqual)
  <|> (symbol "<" *> pure Less)
  <|> (symbol "<=" *> pure LessEqual)