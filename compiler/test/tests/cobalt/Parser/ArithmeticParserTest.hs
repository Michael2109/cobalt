module Parser.ArithmeticParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parser.ExprParser

testArithmeticParserIdentifier :: Test
testArithmeticParserIdentifier = do
    let code = "varName"
    TestCase $ assertEqual code
        (Identifier "varName")
        (case (parse arithmeticParser "" code) of
             Left  _ -> Error
             Right x -> x)

testArithmeticParserClassVariable :: Test
testArithmeticParserClassVariable = do
    let code = "ClassName.varName"
    TestCase $ assertEqual code
        (ClassVariable "ClassName" "varName")
        (case (parse arithmeticParser "" code) of
             Left  _ -> Error
             Right x -> x)

testArithmeticParserNewInstance :: Test
testArithmeticParserNewInstance = do
    let code = "new Integer(10)"
    TestCase $ assertEqual code
        (NewClassInstance (Identifier "Integer") [IntConst 10])
        (case (parse arithmeticParser "" code) of
             Left  _ -> Error
             Right x -> x)

testArithmeticParserMethodCall :: Test
testArithmeticParserMethodCall = do
    let code = "methodName(1,2,3)"
    TestCase $ assertEqual code
        (MethodCall "methodName" [IntConst 1, IntConst 2, IntConst 3])
        (case (parse arithmeticParser "" code) of
             Left  _ -> Error
             Right x -> x)

testArithmeticParserAdd :: Test
testArithmeticParserAdd = do
    let code = "x + 100"
    TestCase $ assertEqual code
        (ABinary Add (Identifier "x") (IntConst 100))
        (case (parse arithmeticParser "" code) of
             Left  _ -> Error
             Right x -> x)

testArithmeticParserSubtract :: Test
testArithmeticParserSubtract = do
    let code = "x - 100"
    TestCase $ assertEqual code
        (ABinary Subtract (Identifier "x") (IntConst 100))
        (case (parse arithmeticParser "" code) of
             Left  _ -> Error
             Right x -> x)

testArithmeticParserMultiply :: Test
testArithmeticParserMultiply = do
    let code = "x * 100"
    TestCase $ assertEqual code
        (ABinary Multiply (Identifier "x") (IntConst 100))
        (case (parse arithmeticParser "" code) of
             Left  _ -> Error
             Right x -> x)

testArithmeticParserDivide :: Test
testArithmeticParserDivide = do
    let code = "x / 100"
    TestCase $ assertEqual code
        (ABinary Divide (Identifier "x") (IntConst 100))
        (case (parse arithmeticParser "" code) of
             Left  _ -> Error
             Right x -> x)
