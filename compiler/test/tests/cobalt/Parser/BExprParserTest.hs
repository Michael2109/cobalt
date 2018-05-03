module Parser.BExprParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.ExprParser

testBExprParser :: Test
testBExprParser = do
    let codeTrue = "True"
    let testTrue = testParseSuccess codeTrue (BoolConst True) bExpr
    let testTrueExpr = testParseSuccess codeTrue (BExprContainer $ BoolConst True) expressionParser'

    let codeFalse = "False"
    let testFalse = testParseSuccess codeFalse (BoolConst False) bExpr
    let testFalseExpr = testParseSuccess codeFalse (BExprContainer $ BoolConst False) expressionParser'

    let codeParens = "(True)"
    let testParens = testParseSuccess codeParens (BoolConst True) bExpr
    let testParensExpr = testParseSuccess codeParens (BExprContainer $ BoolConst True) expressionParser'

    let codeRExpr = "x < 10"
    let testRExpr = testParseSuccess codeRExpr (RBinary Less (Var "x") (IntConst 10)) bExpr
    let testRExprExpr = testParseSuccess codeRExpr (BExprContainer (RBinary Less (Var "x") (IntConst 10))) expressionParser'

    TestList [ testTrue
             , testTrueExpr
             , testFalse
             , testFalseExpr
             , testParens
             , testParensExpr
             , testRExpr
             , testRExprExpr
             ]

testRExprParser :: Test
testRExprParser = do
    let codeGreaterThan = "x > 10"
    let testGreaterThan = testParseSuccess codeGreaterThan (RBinary Greater (Var "x") (IntConst 10)) rExpr
    let testGreaterThanExpr = testParseSuccess codeGreaterThan (BExprContainer $ RBinary Greater (Var "x") (IntConst 10)) expressionParser'

    let codeLessThan = "x < 10"
    let testLessThan = testParseSuccess codeLessThan (RBinary Less (Var "x") (IntConst 10)) rExpr
    let testLessThanExpr = testParseSuccess codeLessThan (BExprContainer $ RBinary Less (Var "x") (IntConst 10)) expressionParser'

    let codeGreaterThanEqual = "x >= 10"
    let testGreaterThanEqual = testParseSuccess codeGreaterThanEqual (RBinary GreaterEqual (Var "x") (IntConst 10)) rExpr
    let testGreaterThanEqualExpr = testParseSuccess codeGreaterThanEqual (BExprContainer $ (RBinary GreaterEqual (Var "x") (IntConst 10))) expressionParser'

    let codeLessThanEqual = "x <= 10"
    let testLessThanEqual = testParseSuccess codeLessThanEqual (RBinary LessEqual (Var "x") (IntConst 10)) rExpr
    let testLessThanEqualExpr = testParseSuccess codeLessThanEqual (BExprContainer $ RBinary LessEqual (Var "x") (IntConst 10)) expressionParser'

    TestList [ testGreaterThan
             , testGreaterThanExpr
             , testLessThan
             , testLessThanExpr
             , testGreaterThanEqual
             , testGreaterThanEqualExpr
             , testLessThanEqual
             , testLessThanEqualExpr
             ]
