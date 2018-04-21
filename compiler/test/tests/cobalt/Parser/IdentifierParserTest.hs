module Parser.IdentifierParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser


testIdentifierParserOneCharacter :: Test
testIdentifierParserOneCharacter = do
    let code = "x"
    TestCase $ assertEqual code
        (Identifier $ Name "x")
        (case (parse identifierParser "" code) of
             Left  e -> error $ show e
             Right x -> x)

{-testIdentifierParserDigitFail :: Test
testIdentifierParserDigitFail = do
    let code = "1"
    TestCase $ assertEqual code
        Error
        (case (parse identifierParser "" code) of
             Left  e -> error $ show e
             Right x -> x)-}

testIdentifierParserContainsUnderscore :: Test
testIdentifierParserContainsUnderscore = do
    let code = "an_identifier"
    TestCase $ assertEqual code
        (Identifier $ Name "an_identifier")
        (case (parse identifierParser "" code) of
             Left  e -> error $ show e
             Right x -> x)

testIdentifierParserContainsDigit :: Test
testIdentifierParserContainsDigit = do
    let code = "a1b2c3"
    TestCase $ assertEqual code
        (Identifier $ Name "a1b2c3")
        (case (parse identifierParser "" code) of
             Left  e -> error $ show e
             Right x -> x)

{-testIdentifierParserStartsDigitFail :: Test
testIdentifierParserStartsDigitFail = do
    let code = "123abc"
    TestCase $ assertEqual code
        Error
        (case (parse identifierParser "" code) of
             Left  e -> error $ show e
             Right x -> x)-}

testIdentifierParserCapital :: Test
testIdentifierParserCapital = do
    let code = "ID"
    TestCase $ assertEqual code
        (Identifier $ Name "ID")
        (case (parse identifierParser "" code) of
             Left  e -> error $ show e
             Right x -> x)
