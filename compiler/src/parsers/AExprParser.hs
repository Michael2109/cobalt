module AExprParser (Parser,
  parens, symbol, rword, scn, identifier, word,
  aTerm, aExpr) where

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
import Block


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

