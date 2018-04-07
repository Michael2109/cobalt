module MethodCallParserTest where

import Test.HUnit
import Text.Megaparsec

import Block
import ExprParser

testMethodCallParser :: Test
testMethodCallParser = do
    let code = "methodCall()"
    TestCase $ assertEqual code
        (MethodCall "methodCall" [])
        (case (parse (methodCallParser) "" code) of
             Left  _ -> Error
             Right x -> x)
