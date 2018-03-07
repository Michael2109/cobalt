module BExprParserTest where

import Test.HUnit

import Text.Megaparsec

import ABBlock
import Block

import BaseParser
import ABExprParser

testBExprParserTrue :: Test
testBExprParserTrue = do
  let code = "True"
  TestCase $ assertEqual code
    (BoolConst True)
    (case (parse (bExpr) "" code) of
      Left  e -> BError
      Right x -> x)

testBExprParserFalse :: Test
testBExprParserFalse = do
  let code = "False"
  TestCase $ assertEqual code
    (BoolConst False)
    (case (parse (bExpr) "" code) of
      Left  e -> BError
      Right x -> x)

testBExprParserFail :: Test
testBExprParserFail = do
  let code = "true"
  TestCase $ assertEqual code
    (BError)
    (case (parse (bExpr) "" code) of
      Left  e -> BError
      Right x -> x)
