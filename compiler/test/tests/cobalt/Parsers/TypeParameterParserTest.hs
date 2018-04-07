module Parsers.TypeParameterParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parsers.ExprParser

testTypeParameterParser :: Test
testTypeParameterParser = do
    let code = "[String]"
    TestCase $ assertEqual code
        (TypeParameter (Identifier "String"))
        (case (parse (typeParameterParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTypeParameterParserMissingLeft :: Test
testTypeParameterParserMissingLeft = do
    let code = "String]"
    TestCase $ assertEqual code
        Error
        (case (parse (typeParameterParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTypeParameterParserMissingRight :: Test
testTypeParameterParserMissingRight = do
    let code = "[String"
    TestCase $ assertEqual code
        Error
        (case (parse (typeParameterParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testTypeParameterParserMissingBoth :: Test
testTypeParameterParserMissingBoth = do
    let code = "String"
    TestCase $ assertEqual code
        Error
        (case (parse (typeParameterParser) "" code) of
             Left  _ -> Error
             Right x -> x)
