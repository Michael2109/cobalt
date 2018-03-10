module ClassVariableParserTest where

import Test.HUnit

import Text.Megaparsec

import ABBlock
import Block

import BaseParser
import ABExprParser
import ExprParser
import Parser


testClassVariableParser :: Test
testClassVariableParser = do
  let code = "objName.varName"
  TestCase $ assertEqual code
    (ClassVariable "objName" "varName")
    (case (parse (classVariableParser) "" code) of
      Left  e -> Error
      Right x -> x)
