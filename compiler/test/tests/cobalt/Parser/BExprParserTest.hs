module Parser.BExprParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testBExprParser :: Test
testBExprParser = do
    let codeTrue = "True"
    let testTrue = TestCase $ assertEqual codeTrue
                       (BoolConst True)
                       (case (parse bExpr "" codeTrue) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeFalse = "False"
    let testFalse = TestCase $ assertEqual codeFalse
                       (BoolConst False)
                       (case (parse bExpr "" codeFalse) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeParens = "(True)"
    let testParens = TestCase $ assertEqual codeParens
                       (BoolConst True)
                       (case (parse bExpr "" codeParens) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeRExpr = "x < 10"
    let testRExpr = TestCase $ assertEqual codeRExpr
                       (RBinary Less (Var "x") (IntConst 10))
                       (case (parse bExpr "" codeRExpr) of
                           Left  e -> error $ show e
                           Right x -> x)

    TestList [testTrue, testFalse, testParens, testRExpr]

testBExprParserExpr :: Test
testBExprParserExpr = do
    let codeTrue = "True"
    let testTrue = TestCase $ assertEqual codeTrue
                       (BExprContainer $ BoolConst True)
                       (case (parse expressionParser' "" codeTrue) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeFalse = "False"
    let testFalse = TestCase $ assertEqual codeFalse
                       (BExprContainer $ BoolConst False)
                       (case (parse expressionParser' "" codeFalse) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeParens = "(True)"
    let testParens = TestCase $ assertEqual codeParens
                       (BExprContainer $ BoolConst True)
                       (case (parse expressionParser' "" codeParens) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeRExpr = "x < 10"
    let testRExpr = TestCase $ assertEqual codeRExpr
                       (BExprContainer (RBinary Less (Var "x") (IntConst 10)))
                       (case (parse expressionParser' "" codeRExpr) of
                           Left  e -> error $ show e
                           Right x -> x)

    TestList [testTrue, testFalse, testParens, testRExpr]
