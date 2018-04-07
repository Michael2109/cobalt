module Parsers.MethodCallParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parsers.ExprParser

testMethodCallParser :: Test
testMethodCallParser = do
    let code = "methodCall()"
    TestCase $ assertEqual code
        (MethodCall "methodCall" [])
        (case (parse (methodCallParser) "" code) of
             Left  _ -> Error
             Right x -> x)
