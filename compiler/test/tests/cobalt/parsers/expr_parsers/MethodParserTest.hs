module MethodParserTest where

import Test.HUnit
import Text.Megaparsec

import Block
import ExprParser

testMethodParser :: Test
testMethodParser = do
    let code = unlines [ "exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (Argument (StringLiteral "Hello world"))])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserEmptyParams :: Test
testMethodParserEmptyParams = do
    let code = unlines [ "exampleMethod (): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [] (Identifier "Int") False [Print (Argument (StringLiteral "Hello world"))])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserMissingParens :: Test
testMethodParserMissingParens = do
    let code = unlines [ "exampleMethod : Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [] (Identifier "Int") False [Print (Argument (StringLiteral "Hello world"))])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserMissingName :: Test
testMethodParserMissingName = do
    let code = unlines [ "(a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        Error
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserMissingReturnType :: Test
testMethodParserMissingReturnType = do
    let code = unlines [ "exampleMethod(a: Int, b: Int)"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        Error
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)
