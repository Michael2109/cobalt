module ParameterParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import ParserExecutor


testParameterParser :: Test
testParameterParser = do
  let code = "x: Int"
  TestCase $ assertEqual code
    (Parameter (Identifier "Int") (Identifier "x"))
    (case (parse (parameterParser) "" code) of
      Left e -> Error
      Right x -> x)

testParameterParserMissingVar :: Test
testParameterParserMissingVar = do
  let code = ": Int"
  TestCase $ assertEqual code
    Error
    (case (parse (parameterParser) "" code) of
      Left e -> Error
      Right x -> x)

testParameterParserMissingType :: Test
testParameterParserMissingType = do
  let code = "x: "
  TestCase $ assertEqual code
    Error
    (case (parse (parameterParser) "" code) of
      Left e -> Error
      Right x -> x)

testParameterParserMissingColon :: Test
testParameterParserMissingColon = do
  let code = "x Int"
  TestCase $ assertEqual code
    Error
    (case (parse (parameterParser) "" code) of
      Left e -> Error
      Right x -> x)
