module Parser.ParameterizedTypeParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parser.ExprParser

testParameterizedTypeParser :: Test
testParameterizedTypeParser = do
    let code = "List[String]"
    TestCase $ assertEqual code
        (ParameterizedType (Identifier "List") (TypeParameter (Identifier "String")))
        (case (parse (parameterizedTypeParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testParameterizedTypeParserLeftMissing :: Test
testParameterizedTypeParserLeftMissing = do
    let code = "ListString]"
    TestCase $ assertEqual code
        Error
        (case (parse (parameterizedTypeParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testParameterizedTypeParserRightMissing :: Test
testParameterizedTypeParserRightMissing = do
    let code = "List[String"
    TestCase $ assertEqual code
        Error
        (case (parse (parameterizedTypeParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testParameterizedTypeParserClassMissing :: Test
testParameterizedTypeParserClassMissing = do
    let code = "[String]"
    TestCase $ assertEqual code
        Error
        (case (parse (parameterizedTypeParser) "" code) of
             Left  _ -> Error
             Right x -> x)
