module ImportParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import ParserExecutor

testImportParserSingle :: Test
testImportParserSingle = do
  let code = "import x"
  TestCase $ assertEqual code
    (Import ["x"])
    (case (parse importParser "" code) of
      Left  e -> Error
      Right x -> x)

testImportParserEmptyFail :: Test
testImportParserEmptyFail = do
  let code = "import "
  TestCase $ assertEqual code
    Error
    (case (parse importParser "" code) of
      Left  e -> Error
      Right x -> x)

testImportParserTwo :: Test
testImportParserTwo = do
  let code = "import x.y"
  TestCase $ assertEqual code
    (Import ["x", "y"])
    (case (parse importParser "" code) of
      Left  e -> Error
      Right x -> x)

testImportParserMultiple :: Test
testImportParserMultiple = do
  let code = "import x.y.z.a.b.c"
  TestCase $ assertEqual code
    (Import ["x", "y", "z", "a", "b", "c"])
    (case (parse importParser "" code) of
      Left  e -> Error
      Right x -> x)

testImportParserStartsDigitFail :: Test
testImportParserStartsDigitFail = do
  let code = "import 1abc"
  TestCase $ assertEqual code
    Error
    (case (parse importParser "" code) of
      Left  e -> Error
      Right x -> x)

testImportParserStartsDigitMultipleFail :: Test
testImportParserStartsDigitMultipleFail = do
  let code = "import abc.xyz.1mno"
  TestCase $ assertEqual code
    Error
    (case (parse importParser "" code) of
      Left  e -> Error
      Right x -> x)

testImportParserCapital :: Test
testImportParserCapital = do
  let code = "import abc.xyz.Name"
  TestCase $ assertEqual code
    (Import ["abc", "xyz", "Name"])
    (case (parse importParser "" code) of
      Left  e -> Error
      Right x -> x)

testImportParserUnderscore :: Test
testImportParserUnderscore = do
  let code = "import abc.xy_z.Name"
  TestCase $ assertEqual code
    (Import ["abc", "xy_z", "Name"])
    (case (parse importParser "" code) of
      Left  e -> Error
      Right x -> x)

testImportParserMultipleUnderscore :: Test
testImportParserMultipleUnderscore = do
  let code = "import dir.sub_dir.Class_Name"
  TestCase $ assertEqual code
    (Import ["dir", "sub_dir", "Class_Name"])
    (case (parse importParser "" code) of
      Left  e -> Error
      Right x -> x)

testImportParserContainsDigit :: Test
testImportParserContainsDigit = do
  let code = "import abc.x1y2z3"
  TestCase $ assertEqual code
    (Import ["abc", "x1y2z3"])
    (case (parse importParser "" code) of
      Left  e -> Error
      Right x -> x)
