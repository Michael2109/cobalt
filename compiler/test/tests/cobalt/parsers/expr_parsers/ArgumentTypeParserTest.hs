module ArgumentTypeParserTest where

import Test.HUnit
import Text.Megaparsec

import Block
import ExprParser

testArgumentTypeParser :: Test
testArgumentTypeParser = do
    let code = "ClassName"
    TestCase $ assertEqual code
        (ArgumentType "ClassName")
        (case (parse (argumentTypeParser) "" code) of
             Left  _ -> Error
             Right x -> x)


testArgumentTypeParserReservedWord :: Test
testArgumentTypeParserReservedWord = do
    let code = "True"
    TestCase $ assertEqual code
        Error
        (case (parse (argumentTypeParser) "" code) of
             Left  _ -> Error
             Right x -> x)
