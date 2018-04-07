{-|
Module      : ParserExecutor
Description : Functions that call the ExprParser functions to generate the AST.
-}
module Parser.ParserExecutor where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Text.Pretty.Simple (pShow)

import Parser.ExprParser

parseTree relativeDir input = parse (expr') "" input

parseFromFile file = runParser expr file <$> readFile file
