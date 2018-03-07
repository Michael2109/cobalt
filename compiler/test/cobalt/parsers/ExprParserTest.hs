module ExprParserTest where


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

-- Boolean Expression tests

testBooleanParserTrue :: Test
testBooleanParserTrue = do
  TestCase $ assertEqual "should correctly pass 'True'"
    (BooleanExpr (BoolConst True))
    (case (parse (booleanParser) "" "True") of
      Left  e -> Empty
      Right x -> x)


testBooleanParserFalse :: Test
testBooleanParserFalse = do
  TestCase $ assertEqual "should correctly pass 'False'"
    (BooleanExpr (BoolConst False))
    (case (parse (booleanParser) "" "False") of
      Left  e -> Empty
      Right x -> x)


testBooleanParserIdentifier :: Test
testBooleanParserIdentifier = do
  TestCase $ assertEqual "should parse as an empty block"
    (Empty)
    (case (parse (booleanParser) "" "true") of
      Left  e -> Empty
      Right x -> x)


testBooleanParserLessThan :: Test
testBooleanParserLessThan = do
  TestCase $ assertEqual "x < y"
    (BooleanExpr (RBinary Less (Var "x") (Var "y")))
    (case (parse (booleanParser) "" "x < y") of
      Left  e -> Empty
      Right x -> x)

testBooleanParserGreaterThan :: Test
testBooleanParserGreaterThan = do
  TestCase $ assertEqual "x > y"
    (BooleanExpr (RBinary Greater (Var "x") (Var "y")))
    (case (parse (booleanParser) "" "x > y") of
      Left  e -> Empty
      Right x -> x)

testBooleanParserLessThanEqual :: Test
testBooleanParserLessThanEqual = do
  TestCase $ assertEqual "x <= y"
    (BooleanExpr (RBinary LessEqual (Var "x") (Var "y")))
    (case (parse (booleanParser) "" "x <= y") of
      Left  e -> Empty
      Right x -> x)

testBooleanParserGreaterThanEqual :: Test
testBooleanParserGreaterThanEqual = do
  TestCase $ assertEqual "x >= y"
    (BooleanExpr (RBinary GreaterEqual (Var "x") (Var "y")))
    (case (parse (booleanParser) "" "x >= y") of
      Left  e -> Empty
      Right x -> x)
