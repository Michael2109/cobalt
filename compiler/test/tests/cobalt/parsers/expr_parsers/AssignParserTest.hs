module AssignParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import ParserExecutor

testAssignParserValWithType :: Test
testAssignParserValWithType = do
  let code = "val x: Int = 1"
  TestCase $ assertEqual code
    (Assign True (Just (Type (Identifier "Int"))) (Identifier "x") (ArithExpr (IntConst 1)))
    (case (parse assignParser "" code) of
      Left  e -> Error
      Right x -> x)

testAssignParserValWithoutType :: Test
testAssignParserValWithoutType = do
  let code = "val x = 1"
  TestCase $ assertEqual code
    (Assign True Nothing (Identifier "x") (ArithExpr (IntConst 1)))
    (case (parse assignParser "" code) of
      Left  e -> Error
      Right x -> x)

testAssignParserWithoutVal :: Test
testAssignParserWithoutVal = do
  let code = "x = 1"
  TestCase $ assertEqual code
    Error
    (case (parse assignParser "" code) of
      Left  e -> Error
      Right x -> x)

testAssignParserVarWithType :: Test
testAssignParserVarWithType = do
  let code = "var x: Int = 1"
  TestCase $ assertEqual code
    (Assign False (Just (Type (Identifier "Int"))) (Identifier "x") (ArithExpr (IntConst 1)))
    (case (parse assignParser "" code) of
      Left  e -> Error
      Right x -> x)

testAssignParserVarWithoutType :: Test
testAssignParserVarWithoutType = do
  let code = "var x = 1"
  TestCase $ assertEqual code
    (Assign False Nothing (Identifier "x") (ArithExpr (IntConst 1)))
    (case (parse assignParser "" code) of
      Left  e -> Error
      Right x -> x)

testAssignParserValWithParameterizedType :: Test
testAssignParserValWithParameterizedType = do
  let code = "val x: Array[String] = 1"
  TestCase $ assertEqual code
    (Assign True (Just (Type (ParameterizedType (Identifier "Array") (TypeParameter (Identifier "String"))))) (Identifier "x") (ArithExpr (IntConst 1)))
    (case (parse assignParser "" code) of
      Left  e -> Error
      Right x -> x)

testAssignParserVarWithParameterizedType :: Test
testAssignParserVarWithParameterizedType = do
  let code = "var x: Array[String] = 1"
  TestCase $ assertEqual code
    (Assign False (Just (Type (ParameterizedType (Identifier "Array") (TypeParameter (Identifier "String"))))) (Identifier "x") (ArithExpr (IntConst 1)))
    (case (parse assignParser "" code) of
      Left  e -> Error
      Right x -> x)
