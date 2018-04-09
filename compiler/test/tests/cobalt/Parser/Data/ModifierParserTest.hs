module Parser.Data.ModifierParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Data.Modifier
import Parser.Data.ModifierParser

testAccessModifierParserPublic :: Test
testAccessModifierParserPublic = do
    let code = "public"
    TestCase $ assertEqual code
        Public
        (case (parse accessModifierParser "" code) of
             Left  _ -> error "Access modifier incorrectly parsed"
             Right x -> x)

testAccessModifierParserProtected :: Test
testAccessModifierParserProtected = do
    let code = "protected"
    TestCase $ assertEqual code
        Protected
        (case (parse accessModifierParser "" code) of
             Left  _ -> error "Access modifier incorrectly parsed"
             Right x -> x)

testAccessModifierParserPrivate :: Test
testAccessModifierParserPrivate = do
    let code = "private"
    TestCase $ assertEqual code
        Private
        (case (parse accessModifierParser "" code) of
             Left  _ -> error "Access modifier incorrectly parsed"
             Right x -> x)

testAbstractModifierParser :: Test
testAbstractModifierParser = do
    let code = "abstract"
    TestCase $ assertEqual code
        Abstract
        (case (parse abstractModifierParser "" code) of
             Left  _ -> error "Access modifier incorrectly parsed"
             Right x -> x)

testFinalModifierParser :: Test
testFinalModifierParser = do
    let code = "final"
    TestCase $ assertEqual code
        Final
        (case (parse finalModifierParser "" code) of
             Left  _ -> error "Access modifier incorrectly parsed"
             Right x -> x)