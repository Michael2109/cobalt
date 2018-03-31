module MethodCallParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import ParserExecutor


testMethodCallParser :: Test
testMethodCallParser = do
  let code = "methodCall()"
  TestCase $ assertEqual code
    (MethodCall "methodCall" [])
    (case (parse (methodCallParser) "" code) of
      Left  e -> Error
      Right x -> x)
