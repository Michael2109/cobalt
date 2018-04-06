module ObjectMethodCallParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import ParserExecutor

testObjectMethodCallParserThis :: Test
testObjectMethodCallParserThis = do
    let code = "this.methodCall()"
    TestCase $ assertEqual code
        (ThisMethodCall "methodCall" [])
        (case (parse (thisMethodCallParser) "" code) of
             Left  e -> Error
             Right x -> x)

testObjectMethodCallParserObject :: Test
testObjectMethodCallParserObject = do
    let code = "obj.methodCall()"
    TestCase $ assertEqual code
        (ObjectMethodCall "obj" "methodCall" [])
        (case (parse (objectMethodCallParser) "" code) of
             Left  e -> Error
             Right x -> x)

testObjectMethodCallParserSuper :: Test
testObjectMethodCallParserSuper = do
    let code = "super.methodCall()"
    TestCase $ assertEqual code
        (SuperMethodCall "methodCall" [])
        (case (parse (superMethodCallParser) "" code) of
             Left  e -> Error
             Right x -> x)
