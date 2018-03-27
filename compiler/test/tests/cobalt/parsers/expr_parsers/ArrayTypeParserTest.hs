module ArrayTypeParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import Parser

testArrayTypeParser :: Test
testArrayTypeParser = do
  let code = "[Type]"
  TestCase $ assertEqual code
    (ArrayType "Type")
    (case (parse arrayType "" code) of
      Left  e -> Error
      Right x -> x)

testArrayTypeParserEmptyFail :: Test
testArrayTypeParserEmptyFail = do
  let code = "[]"
  TestCase $ assertEqual code
    Error
    (case (parse arrayType "" code) of
      Left  e -> Error
      Right x -> x)

testArrayTypeParserNoOpenFail :: Test
testArrayTypeParserNoOpenFail = do
  let code = "[Type"
  TestCase $ assertEqual code
    Error
    (case (parse arrayType "" code) of
      Left  e -> Error
      Right x -> x)

testArrayTypeParserNoCloseFail :: Test
testArrayTypeParserNoCloseFail = do
  let code = "Type]"
  TestCase $ assertEqual code
    Error
    (case (parse arrayType "" code) of
      Left  e -> Error
      Right x -> x)

testArrayTypeParserStartsDigitFail :: Test
testArrayTypeParserStartsDigitFail = do
  let code = "[1Type]"
  TestCase $ assertEqual code
    Error
    (case (parse arrayType "" code) of
      Left  e -> Error
      Right x -> x)

testArrayTypeParserContainsDigit :: Test
testArrayTypeParserContainsDigit = do
  let code = "[a1b2c3]"
  TestCase $ assertEqual code
    (ArrayType "a1b2c3")
    (case (parse arrayType "" code) of
      Left  e -> Error
      Right x -> x)

testArrayTypeParserContainsUnderscore :: Test
testArrayTypeParserContainsUnderscore = do
  let code = "[my_type]"
  TestCase $ assertEqual code
    (ArrayType "my_type")
    (case (parse arrayType "" code) of
      Left  e -> Error
      Right x -> x)
