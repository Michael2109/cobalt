module Parser.ParameterParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parser.ExprParser

testParameterParser :: Test
testParameterParser = do
    let code = "x: Int"
    TestCase $ assertEqual code
        (Parameter (Identifier "Int") (Identifier "x"))
        (case (parse (parameterParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testParameterParserMissingVar :: Test
testParameterParserMissingVar = do
    let code = ": Int"
    TestCase $ assertEqual code
        Error
        (case (parse (parameterParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testParameterParserMissingType :: Test
testParameterParserMissingType = do
    let code = "x: "
    TestCase $ assertEqual code
        Error
        (case (parse (parameterParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testParameterParserMissingColon :: Test
testParameterParserMissingColon = do
    let code = "x Int"
    TestCase $ assertEqual code
        Error
        (case (parse (parameterParser) "" code) of
             Left  _ -> Error
             Right x -> x)
