module Parser.AExprParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.Parser

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testAParser :: Test
testAParser = do
    let codeAdd = "x + 100"
    let testAdd = testParseSuccess codeAdd (ABinary Add (IntConst 100) (IntConst 100)) aExpr

    let codeSubtract = "x - 100"
    let testSubtract = testParseSuccess codeSubtract (ABinary Subtract (IntConst 100) (IntConst 100)) aExpr

    let codeMultiply = "x * 100"
    let testMultiply = testParseSuccess codeMultiply (ABinary Multiply (IntConst 100) (IntConst 100)) aExpr

    let codeDivide = "x / 100"
    let testDivide = testParseSuccess codeDivide (ABinary Divide (IntConst 100) (IntConst 100)) aExpr

    let codeMixed = "x / 100 * y + 200 - z"
    let testMixed = testParseSuccess codeMixed (ABinary Subtract (ABinary Add (ABinary Multiply (ABinary Divide (IntConst 100) (IntConst 100)) (IntConst 100)) (IntConst 200)) (IntConst 100)) aExpr

    let codeParens = "x / 100 * (y + 200) - z"
    let testParens = testParseSuccess codeParens (ABinary Subtract (ABinary Multiply (ABinary Divide (Var "x") (IntConst 100)) (ABinary Add (Var "y") (IntConst 200))) (Var "z")) aExpr

    let codeNegative = "- 100"
    let testNegative = testParseSuccess codeNegative (Neg (IntConst 100)) aExpr

    TestList [ testAdd
             , testSubtract
             , testMultiply
             , testDivide
             , testMixed
             , testParens
             , testNegative
             ]
