module AExprParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser


testAExprParserVar :: Test
testAExprParserVar = do
    let code = "x"
    TestCase $ assertEqual code
        (Identifier "x")
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
        (Neg (Identifier "x"))
        (case (parse (aExpr) "" code) of
             Left  e -> AError
             Right x -> x)

-- TODO write test for parenthesis
