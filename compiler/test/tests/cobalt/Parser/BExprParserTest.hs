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


testRExprParser :: Test
testRExprParser = do
    let codeGreaterThan = "x > 10"
    let testGreaterThan = TestCase $ assertEqual codeGreaterThan
                       (RBinary Greater (Var "x") (IntConst 10))
                       (case (parse rExpr "" codeGreaterThan) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeLessThan = "x < 10"
    let testLessThan = TestCase $ assertEqual codeLessThan
                       (RBinary Less (Var "x") (IntConst 10))
                       (case (parse rExpr "" codeLessThan) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeGreaterThanEqual = "x >= 10"
    let testGreaterThanEqual = TestCase $ assertEqual codeGreaterThanEqual
                       (RBinary GreaterEqual (Var "x") (IntConst 10))
                       (case (parse rExpr "" codeGreaterThanEqual) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeLessThanEqual = "x <= 10"
    let testLessThanEqual = TestCase $ assertEqual codeLessThanEqual
                       (RBinary LessEqual (Var "x") (IntConst 10))
                       (case (parse rExpr "" codeLessThanEqual) of
                           Left  e -> error $ show e
                           Right x -> x)

    TestList [ testGreaterThan
             , testLessThan
             , testGreaterThanEqual
             , testLessThanEqual
             ]

testRExprParserExpr :: Test
testRExprParserExpr = do
    let codeGreaterThan = "x > 10"
    let testGreaterThan = TestCase $ assertEqual codeGreaterThan
                       (BExprContainer $ RBinary Greater (Var "x") (IntConst 10))
                       (case (parse expressionParser' "" codeGreaterThan) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeLessThan = "x < 10"
    let testLessThan = TestCase $ assertEqual codeLessThan
                       (BExprContainer $ RBinary Less (Var "x") (IntConst 10))
                       (case (parse expressionParser' "" codeLessThan) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeGreaterThanEqual = "x >= 10"
    let testGreaterThanEqual = TestCase $ assertEqual codeGreaterThanEqual
                       (BExprContainer $ (RBinary GreaterEqual (Var "x") (IntConst 10)))
                       (case (parse expressionParser' "" codeGreaterThanEqual) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeLessThanEqual = "x <= 10"
    let testLessThanEqual = TestCase $ assertEqual codeLessThanEqual
                       (BExprContainer $ RBinary LessEqual (Var "x") (IntConst 10))
                       (case (parse expressionParser' "" codeLessThanEqual) of
                           Left  e -> error $ show e
                           Right x -> x)

    TestList [ testGreaterThan
             , testLessThan
             , testGreaterThanEqual
             , testLessThanEqual
             ]
