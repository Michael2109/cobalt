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

import ABExprParserPrivate
import BaseParser
import ABBlock
import SymbolTable
