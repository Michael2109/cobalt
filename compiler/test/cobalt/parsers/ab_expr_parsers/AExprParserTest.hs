module AExprParserTest where

import Test.HUnit

import Text.Megaparsec

import ABBlock
import Block

import BaseParser
import ABExprParser


testAExprParserVar :: Test
testAExprParserVar = do
  let code = "x"
  TestCase $ assertEqual code
    (Var "x")
    (case (parse (aExpr) "" code) of
      Left  e -> AError
      Right x -> x)

testAExprParserInt :: Test
testAExprParserInt = do
  let code = "1000"
  TestCase $ assertEqual code
    (IntConst 1000)
    (case (parse (aExpr) "" code) of
      Left  e -> AError
      Right x -> x)

testAExprParserNeg :: Test
testAExprParserNeg = do
  let code = "-x"
  TestCase $ assertEqual code
    (Neg (Var "x"))
    (case (parse (aExpr) "" code) of
      Left  e -> AError
      Right x -> x)

-- TODO write test for parenthesis