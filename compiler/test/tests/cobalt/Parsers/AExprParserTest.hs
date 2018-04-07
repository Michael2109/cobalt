module Parsers.AExprParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parsers.ExprParser

testAExprParserVar :: Test
testAExprParserVar = do
    let code = "x"
    TestCase $ assertEqual code
        (Identifier "x")
        (case (parse (aExpr) "" code) of
             Left  _ -> AError
             Right x -> x)

testAExprParserInt :: Test
testAExprParserInt = do
    let code = "1000"
    TestCase $ assertEqual code
        (IntConst 1000)
        (case (parse (aExpr) "" code) of
             Left  _ -> AError
             Right x -> x)

testAExprParserNeg :: Test
testAExprParserNeg = do
    let code = "-x"
    TestCase $ assertEqual code
        (Neg (Identifier "x"))
        (case (parse (aExpr) "" code) of
             Left  _ -> AError
             Right x -> x)

-- TODO write test for parenthesis
