module Parser.AExprParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

import TestUtil.ParserTestUtil
import AST.AST
import Parser.ExprParser

testAExprParser :: Test
testAExprParser = do
    let codeAdd = "x + 100"
    let testAdd = testParseSuccess codeAdd (ABinary Add (Var "x") (IntConst 100)) aExpr

    let codeSubtract = "x - 100"
    let testSubtract = testParseSuccess codeSubtract (ABinary Subtract (Var "x") (IntConst 100)) aExpr

    let codeMultiply = "x * 100"
    let testMultiply = testParseSuccess codeMultiply (ABinary Multiply (Var "x") (IntConst 100)) aExpr

    let codeDivide = "x / 100"
    let testDivide = testParseSuccess codeDivide (ABinary Divide (Var "x") (IntConst 100)) aExpr

    let codeMixed = "x / 100 * y + 200 - z"
    let testMixed = testParseSuccess codeMixed (ABinary Subtract (ABinary Add (ABinary Multiply (ABinary Divide (Var "x") (IntConst 100)) (Var "y")) (IntConst 200)) (Var "z")) aExpr

    let codeNegative = "- 100"
    let testNegative = testParseSuccess codeNegative (Neg (IntConst 100)) aExpr

    TestList [ testAdd
             , testSubtract
             , testMultiply
             , testDivide
             , testMixed
             , testNegative
             ]
