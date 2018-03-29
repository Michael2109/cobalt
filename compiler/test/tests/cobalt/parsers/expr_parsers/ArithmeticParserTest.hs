module ArithmeticParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import Parser

testArithmeticParserIdentifier :: Test
testArithmeticParserIdentifier = do
  let code = "varName"
  TestCase $ assertEqual code
    (ArithExpr (Identifier "varName"))
    (case (parse arithmeticParser "" code) of
      Left  e -> Error
      Right x -> x)

testArithmeticParserClassVariable :: Test
testArithmeticParserClassVariable = do
  let code = "ClassName.varName"
  TestCase $ assertEqual code
    (ArithExpr (ClassVariable "ClassName" "varName"))
    (case (parse arithmeticParser "" code) of
      Left  e -> Error
      Right x -> x)

testArithmeticParserNewInstance :: Test
testArithmeticParserNewInstance = do
  let code = "new Integer(10)"
  TestCase $ assertEqual code
    (ArithExpr (NewClassInstance (Identifier "Integer") [Argument (ArithExpr (IntConst 10))]))
    (case (parse arithmeticParser "" code) of
      Left  e -> Error
      Right x -> x)

testArithmeticParserMethodCall :: Test
testArithmeticParserMethodCall = do
  let code = "methodName(1,2,3)"
  TestCase $ assertEqual code
    (ArithExpr (MethodCall "methodName" [Argument (ArithExpr (IntConst 1)),Argument (ArithExpr (IntConst 2)),Argument (ArithExpr (IntConst 3))]))
    (case (parse arithmeticParser "" code) of
      Left  e -> Error
      Right x -> x)

testArithmeticParserAdd :: Test
testArithmeticParserAdd = do
  let code = "x + 100"
  TestCase $ assertEqual code
    (ArithExpr (ABinary Add (Identifier "x") (IntConst 100)))
    (case (parse arithmeticParser "" code) of
      Left  e -> Error
      Right x -> x)

testArithmeticParserSubtract :: Test
testArithmeticParserSubtract = do
  let code = "x - 100"
  TestCase $ assertEqual code
    (ArithExpr (ABinary Subtract (Identifier "x") (IntConst 100)))
    (case (parse arithmeticParser "" code) of
      Left  e -> Error
      Right x -> x)

testArithmeticParserMultiply :: Test
testArithmeticParserMultiply = do
  let code = "x * 100"
  TestCase $ assertEqual code
    (ArithExpr (ABinary Multiply (Identifier "x") (IntConst 100)))
    (case (parse arithmeticParser "" code) of
      Left  e -> Error
      Right x -> x)

testArithmeticParserDivide :: Test
testArithmeticParserDivide = do
  let code = "x / 100"
  TestCase $ assertEqual code
    (ArithExpr (ABinary Divide (Identifier "x") (IntConst 100)))
    (case (parse arithmeticParser "" code) of
      Left  e -> Error
      Right x -> x)