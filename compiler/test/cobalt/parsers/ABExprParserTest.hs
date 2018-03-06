module ABExprParserTest where

import Test.HUnit

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Void
import Data.Char (isAlphaNum)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Text.Pretty.Simple (pShow)

import ABBlock
import Block

import BaseParser
import ABExprParser
import ExprParser
import Parser

testBooleanParserTrue :: Test
testBooleanParserTrue = do
  let ast = (parse (booleanParser) "" "True")

  TestCase $ assertEqual "should correctly pass 'True'"
    (BooleanExpr (BoolConst True))
    (case ast of
      Left  e -> Empty
      Right x -> x)


testBooleanParserFalse :: Test
testBooleanParserFalse = do
  let ast = (parse (booleanParser) "" "False")

  TestCase $ assertEqual "should correctly pass 'False'"
    (BooleanExpr (BoolConst False))
    (case ast of
      Left  e -> Empty
      Right x -> x)



testBooleanParserFail :: Test
testBooleanParserFail = do
  let ast = (parse (booleanParser) "" "true")

  TestCase $ assertEqual "should fail with 'true'"
    (BooleanExpr (BoolConst True))
    (case ast of
      Left  e -> Empty
      Right x -> x)