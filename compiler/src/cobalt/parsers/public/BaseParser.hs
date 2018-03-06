{-|
Module      : BaseParser
Description : Contains parsing functions used in all other parsers.
Generally a lower level parser for words etc.
-}
module BaseParser (Parser, scn, symbol, integer, rword, rws, parens, word, identifier, valueToken) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Data.Char (isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Text.Pretty.Simple (pShow)

import BaseParserPrivate
import Block