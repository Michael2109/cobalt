module Parsers.ObjectMethodCallParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parsers.ExprParser

testObjectMethodCallParserThis :: Test
testObjectMethodCallParserThis = do
    let code = "this.methodCall()"
    TestCase $ assertEqual code
        (ThisMethodCall "methodCall" [])
        (case (parse (thisMethodCallParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectMethodCallParserObject :: Test
testObjectMethodCallParserObject = do
    let code = "obj.methodCall()"
    TestCase $ assertEqual code
        (ObjectMethodCall "obj" "methodCall" [])
        (case (parse (objectMethodCallParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testObjectMethodCallParserSuper :: Test
testObjectMethodCallParserSuper = do
    let code = "super.methodCall()"
    TestCase $ assertEqual code
        (SuperMethodCall "methodCall" [])
        (case (parse (superMethodCallParser) "" code) of
             Left  _ -> Error
             Right x -> x)
