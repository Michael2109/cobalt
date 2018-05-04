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
    let testTrueExpr = testParseSuccess codeTrue (BExprAsExpr $ BoolConst True) expressionParser'

    let codeFalse = "False"
    let testFalse = testParseSuccess codeFalse (BoolConst False) bExpr
    let testFalseExpr = testParseSuccess codeFalse (BExprAsExpr $ BoolConst False) expressionParser'

    let codeParens = "(True)"
    let testParens = testParseSuccess codeParens (BoolConst True) bExpr
    let testParensExpr = testParseSuccess codeParens (BExprAsExpr $ BoolConst True) expressionParser'

    let codeRExpr = "x < 10"
    let testRExpr = testParseSuccess codeRExpr (BoolConst True) bExpr
    let testRExprExpr = testParseSuccess codeRExpr (BExprAsExpr $ BoolConst True) expressionParser'

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
    let testGreaterThan = testParseSuccess codeGreaterThan (RBinary Greater (ExprAsAExpr (Identifier (Name "x"))) (IntConst 10)) rExpr
    let testGreaterThanExpr = testParseSuccess codeGreaterThan (BExprAsExpr $ BoolConst True) expressionParser'

    let codeLessThan = "x < 10"
    let testLessThan = testParseSuccess codeLessThan (RBinary Less (ExprAsAExpr (Identifier (Name "x"))) (IntConst 10)) rExpr
    let testLessThanExpr = testParseSuccess codeLessThan (BExprAsExpr $ BoolConst True) expressionParser'

    let codeGreaterThanEqual = "x >= 10"
    let testGreaterThanEqual = testParseSuccess codeGreaterThanEqual (RBinary GreaterEqual (ExprAsAExpr (Identifier (Name "x"))) (IntConst 10)) rExpr
    let testGreaterThanEqualExpr = testParseSuccess codeGreaterThanEqual (BExprAsExpr $ BoolConst True) expressionParser'

    let codeLessThanEqual = "x <= 10"
    let testLessThanEqual = testParseSuccess codeLessThanEqual (RBinary LessEqual (ExprAsAExpr (Identifier (Name "x"))) (IntConst 10)) rExpr
    let testLessThanEqualExpr = testParseSuccess codeLessThanEqual (BExprAsExpr $ BoolConst True) expressionParser'

    TestList [ testGreaterThan
             , testGreaterThanExpr
             , testLessThan
             , testLessThanExpr
             , testGreaterThanEqual
             , testGreaterThanEqualExpr
             , testLessThanEqual
             , testLessThanEqualExpr
             ]
