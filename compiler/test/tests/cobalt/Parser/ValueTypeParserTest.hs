module Parser.ValueTypeParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parser.ExprParser
{-

testValueTypeParserOneCharacter :: Test
testValueTypeParserOneCharacter = do
    let code = "x"
    TestCase $ assertEqual code
        (Type (Identifier "x"))
        (case (parse valueTypeParser "" code) of
             Left  _ -> Error
             Right x -> x)

testValueTypeParserDigitFail :: Test
testValueTypeParserDigitFail = do
    let code = "1"
    TestCase $ assertEqual code
        Error
        (case (parse valueTypeParser "" code) of
             Left  _ -> Error
             Right x -> x)

testValueTypeParserContainsUnderscore :: Test
testValueTypeParserContainsUnderscore = do
    let code = "an_ValueType"
    TestCase $ assertEqual code
        (Type (Identifier "an_ValueType"))
        (case (parse valueTypeParser "" code) of
             Left  _ -> Error
             Right x -> x)

testValueTypeParserContainsDigit :: Test
testValueTypeParserContainsDigit = do
    let code = "a1b2c3"
    TestCase $ assertEqual code
        (Type (Identifier "a1b2c3"))
        (case (parse valueTypeParser "" code) of
             Left  _ -> Error
             Right x -> x)

testValueTypeParserStartsDigitFail :: Test
testValueTypeParserStartsDigitFail = do
    let code = "123abc"
    TestCase $ assertEqual code
        Error
        (case (parse valueTypeParser "" code) of
             Left  _ -> Error
             Right x -> x)

testValueTypeParserCapital :: Test
testValueTypeParserCapital = do
    let code = "ID"
    TestCase $ assertEqual code
        (Type (Identifier "ID"))
        (case (parse valueTypeParser "" code) of
             Left  _ -> Error
             Right x -> x)
-}
