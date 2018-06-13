module Parser.AExprParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testAParser :: Test
testAParser = do
    let codeAdd = "x + 100"
    let testAdd = testParseSuccess codeAdd (ABinary Add (Identifier (Name "x")) (IntConst 100)) expressionParser'

    let codeSubtract = "x - 100"
    let testSubtract = testParseSuccess codeSubtract (ABinary Subtract (Identifier (Name "x")) (IntConst 100)) expressionParser'

    let codeMultiply = "x * 100"
    let testMultiply = testParseSuccess codeMultiply (ABinary Multiply (Identifier (Name "x")) (IntConst 100)) expressionParser'

    let codeDivide = "x / 100"
    let testDivide = testParseSuccess codeDivide (ABinary Divide (Identifier (Name "x")) (IntConst 100)) expressionParser'

    let codeMixed = "x / 100 * y + 200 - z"
    let testMixed = testParseSuccess codeMixed (ABinary Subtract (ABinary Add (ABinary Multiply (ABinary Divide (Identifier (Name "x")) (IntConst 100)) (Identifier (Name "y"))) (IntConst 200)) (Identifier (Name "z"))) expressionParser'

    let codeParens = "x / 100 * (y + 200) - z"
    let testParens = testParseSuccess codeParens (ABinary Subtract (ABinary Multiply (ABinary Divide (Identifier (Name "x")) (IntConst 100)) (ABinary Add (Identifier (Name "y")) (IntConst 200))) (Identifier (Name "z"))) expressionParser'

    let codeNegative = "- 100"
    let testNegative = testParseSuccess codeNegative (Neg (IntConst 100)) expressionParser'

    let codeAddDouble = "1.0 + 2.0"
    let testAddDouble = testParseSuccess codeAddDouble (ABinary Add (DoubleConst 1.0) (DoubleConst 2.0)) expressionParser'

    let codeAddDoubleInt = "1.0 + 2"
    let testAddDoubleInt = testParseSuccess codeAddDoubleInt (ABinary Add (DoubleConst 1.0) (IntConst 2)) expressionParser'

    let codeAddFloatFloat = "1.8f + 2f"
    let testAddFloatFloat = testParseSuccess codeAddFloatFloat (ABinary Add (FloatConst 1.8) (FloatConst 2.0)) expressionParser'

    let codeAddFloatFloatCapital = "1.8F + 2F"
    let testAddFloatFloatCapital = testParseSuccess codeAddFloatFloatCapital (ABinary Add (FloatConst 1.8) (FloatConst 2.0)) expressionParser'

    TestList [ testAdd
             , testSubtract
             , testMultiply
             , testDivide
             , testMixed
             , testParens
             , testNegative
             , testAddDouble
             , testAddDoubleInt
             , testAddFloatFloat
             , testAddFloatFloatCapital
             ]
