module BooleanParserTest where

import Test.HUnit

import Text.Megaparsec

import ABBlock
import Block

import BaseParser
import ABExprParser
import ExprParser
import Parser

-- Boolean Expression tests

testBooleanParserTrue :: Test
testBooleanParserTrue = do
  let code = "True"
  TestCase $ assertEqual code
    (BooleanExpr (BoolConst True))
    (case (parse (booleanParser) "" code) of
      Left  e -> Empty
      Right x -> x)


testBooleanParserFalse :: Test
testBooleanParserFalse = do
  let code = "False"
  TestCase $ assertEqual code
    (BooleanExpr (BoolConst False))
    (case (parse (booleanParser) "" code) of
      Left  e -> Empty
      Right x -> x)


testBooleanParserIdentifier :: Test
testBooleanParserIdentifier = do
  let code = "true"
  TestCase $ assertEqual code
    (Empty)
    (case (parse (booleanParser) "" code) of
      Left  e -> Empty
      Right x -> x)


testBooleanParserLessThan :: Test
testBooleanParserLessThan = do
  let code = "x < y"
  TestCase $ assertEqual code
    (BooleanExpr (RBinary Less (Var "x") (Var "y")))
    (case (parse (booleanParser) "" code) of
      Left  e -> Empty
      Right x -> x)

testBooleanParserGreaterThan :: Test
testBooleanParserGreaterThan = do
  let code = "x > y"
  TestCase $ assertEqual code
    (BooleanExpr (RBinary Greater (Var "x") (Var "y")))
    (case (parse (booleanParser) "" code) of
      Left  e -> Empty
      Right x -> x)

testBooleanParserLessThanEqual :: Test
testBooleanParserLessThanEqual = do
  let code = "x <= y"
  TestCase $ assertEqual code
    (BooleanExpr (RBinary LessEqual (Var "x") (Var "y")))
    (case (parse (booleanParser) "" code) of
      Left  e -> Empty
      Right x -> x)

testBooleanParserGreaterThanEqual :: Test
testBooleanParserGreaterThanEqual = do
  let code = "x >= y"
  TestCase $ assertEqual code
    (BooleanExpr (RBinary GreaterEqual (Var "x") (Var "y")))
    (case (parse (booleanParser) "" code) of
      Left  e -> Empty
      Right x -> x)
