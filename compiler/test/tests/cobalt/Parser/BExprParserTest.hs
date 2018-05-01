module Parser.BExprParserTest where

import Test.HUnit
import Text.Megaparsec


import Parser.ExprParser

{-
testBExprParserTrue :: Test
testBExprParserTrue = do
    let code = "True"
    TestCase $ assertEqual code
        (BoolConst True)
        (case (parse (bExpr) "" code) of
             Left  _ -> BError
             Right x -> x)

testBExprParserFalse :: Test
testBExprParserFalse = do
    let code = "False"
    TestCase $ assertEqual code
        (BoolConst False)
        (case (parse (bExpr) "" code) of
             Left  _ -> BError
             Right x -> x)

testBExprParserFail :: Test
testBExprParserFail = do
    let code = "true"
    TestCase $ assertEqual code
        (Identifier "true")
        (case (parse (bExpr) "" code) of
             Left  _ -> BError
             Right x -> x)

testBExprParenthesesConsumerParser :: Test
testBExprParenthesesConsumerParser = do
    let code = "(((True)))"
    TestCase $ assertEqual code
        (BoolConst True)
        (case (parse (bExpr) "" code) of
             Left  _ -> BError
             Right x -> x)
-}
