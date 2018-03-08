module ArgumentParserTest where

import Test.HUnit

import Text.Megaparsec

import ABBlock
import Block

import BaseParser
import ABExprParser
import ExprParser
import Parser

testArgumentParserIdentifier :: Test
testArgumentParserIdentifier = do
  let code = "Test"
  TestCase $ assertEqual code
    (Argument (Identifier "Test"))
    (case (parse (argumentParser) "" code) of
      Left  e -> Error
      Right x -> x)

testArgumentParserBoolTrue :: Test
testArgumentParserBoolTrue = do
  let code = "True"
  TestCase $ assertEqual code
    (Argument (BooleanExpr $ BoolConst True))
    (case (parse (argumentParser) "" code) of
      Left  e -> Error
      Right x -> x)

testArgumentParserBoolFalse :: Test
testArgumentParserBoolFalse = do
  let code = "False"
  TestCase $ assertEqual code
    (Argument (BooleanExpr $ BoolConst False))
    (case (parse (argumentParser) "" code) of
      Left  e -> Error
      Right x -> x)