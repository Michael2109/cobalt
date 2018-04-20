module Parser.ArgumentParserTest where

import Test.HUnit
import Text.Megaparsec


import Parser.ExprParser

{-
testArgumentParserIdentifier :: Test
testArgumentParserIdentifier = do
    let code = "Test"
    TestCase $ assertEqual code
        (Identifier "Test")
        (case (parse (argumentParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testArgumentParserBoolTrue :: Test
testArgumentParserBoolTrue = do
    let code = "True"
    TestCase $ assertEqual code
        (BoolConst True)
        (case (parse (argumentParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testArgumentParserBoolFalse :: Test
testArgumentParserBoolFalse = do
    let code = "False"
    TestCase $ assertEqual code
        (BoolConst False)
        (case (parse (argumentParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testArgumentParserObjectVar :: Test
testArgumentParserObjectVar = do
    let code = "anObject.variable"
    TestCase $ assertEqual code
        (ClassVariable "anObject" "variable")
        (case (parse (argumentParser) "" code) of
             Left  _ -> Error
             Right x -> x)
-}
