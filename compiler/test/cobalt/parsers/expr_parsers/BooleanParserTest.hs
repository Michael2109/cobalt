module BooleanParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import Parser

-- Boolean Expression tests
testBooleanParserTrue :: Test
testBooleanParserTrue = do
  let code = "True"
  TestCase $ assertEqual code
    (BooleanExpr (BoolConst True))
    (case (parse (booleanParser) "" code) of
      Left  e -> Error
      Right x -> x)

testBooleanParserFalse :: Test
testBooleanParserFalse = do
  let code = "False"
  TestCase $ assertEqual code
    (BooleanExpr (BoolConst False))
    (case (parse (booleanParser) "" code) of
      Left  e -> Error
      Right x -> x)

testBooleanParserIdentifier :: Test
testBooleanParserIdentifier = do
  let code = "true"
  TestCase $ assertEqual code
    (Error)
    (case (parse (booleanParser) "" code) of
      Left  e -> Error
      Right x -> x)

-- Less than

testBooleanParserLessThanVar :: Test
testBooleanParserLessThanVar = do
  let code = "x < y"
  TestCase $ assertEqual code
    (BooleanExpr (RBinary Less (Var "x") (Var "y")))
    (case (parse (booleanParser) "" code) of
      Left  e -> Error
      Right x -> x)

testBooleanParserLessThanInt :: Test
testBooleanParserLessThanInt = do
  let code = "100 < 300"
  TestCase $ assertEqual code
    (BooleanExpr (RBinary Less (IntConst 100) (IntConst 300)))
    (case (parse (booleanParser) "" code) of
      Left  e -> Error
      Right x -> x)

-- Greater than

testBooleanParserGreaterThanVar :: Test
testBooleanParserGreaterThanVar = do
  let code = "x > y"
  TestCase $ assertEqual code
    (BooleanExpr (RBinary Greater (Var "x") (Var "y")))
    (case (parse (booleanParser) "" code) of
      Left  e -> Error
      Right x -> x)

testBooleanParserGreaterThanInt :: Test
testBooleanParserGreaterThanInt = do
  let code = "100 > 300"
  TestCase $ assertEqual code
    (BooleanExpr (RBinary Greater (IntConst 100) (IntConst 300)))
    (case (parse (booleanParser) "" code) of
      Left  e -> Error
      Right x -> x)

-- Less than / equal to

testBooleanParserLessThanEqualVar :: Test
testBooleanParserLessThanEqualVar = do
  let code = "x <= y"
  TestCase $ assertEqual code
    (BooleanExpr (RBinary LessEqual (Var "x") (Var "y")))
    (case (parse (booleanParser) "" code) of
      Left  e -> Error
      Right x -> x)

testBooleanParserLessThanEqualInt :: Test
testBooleanParserLessThanEqualInt = do
  let code = "100 <= 300"
  TestCase $ assertEqual code
    (BooleanExpr (RBinary LessEqual (IntConst 100) (IntConst 300)))
    (case (parse (booleanParser) "" code) of
      Left  e -> Error
      Right x -> x)

-- Greater than / equal to
testBooleanParserGreaterThanEqualVar :: Test
testBooleanParserGreaterThanEqualVar = do
  let code = "x >= y"
  TestCase $ assertEqual code
    (BooleanExpr (RBinary GreaterEqual (Var "x") (Var "y")))
    (case (parse (booleanParser) "" code) of
      Left  e -> Error
      Right x -> x)

testBooleanParserGreaterThanEqualInt :: Test
testBooleanParserGreaterThanEqualInt = do
  let code = "100 >= 300"
  TestCase $ assertEqual code
    (BooleanExpr (RBinary GreaterEqual (IntConst 100) (IntConst 300)))
    (case (parse (booleanParser) "" code) of
      Left  e -> Error
      Right x -> x)