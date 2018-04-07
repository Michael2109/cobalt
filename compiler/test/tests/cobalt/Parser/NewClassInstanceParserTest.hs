module Parser.NewClassInstanceParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parser.ExprParser

testNewClassInstanceParserNoArgs :: Test
testNewClassInstanceParserNoArgs = do
    let code = "new ClassName()"
    TestCase $ assertEqual code
        (NewClassInstance (Identifier "ClassName") [])
        (case (parse newClassInstanceParser "" code) of
             Left  _ -> Error
             Right x -> x)

testNewClassInstanceParserNoArgsUnderscore :: Test
testNewClassInstanceParserNoArgsUnderscore = do
    let code = "new Class_Name()"
    TestCase $ assertEqual code
        (NewClassInstance (Identifier "Class_Name") [])
        (case (parse newClassInstanceParser "" code) of
             Left  _ -> Error
             Right x -> x)

testNewClassInstanceParserNoArgsLowerCase :: Test
testNewClassInstanceParserNoArgsLowerCase = do
    let code = "new className()"
    TestCase $ assertEqual code
        (NewClassInstance (Identifier "className") [])
        (case (parse newClassInstanceParser "" code) of
             Left  _ -> Error
             Right x -> x)

testNewClassInstanceParserNewUpperCase :: Test
testNewClassInstanceParserNewUpperCase = do
    let code = "NEW className()"
    TestCase $ assertEqual code
        Error
        (case (parse newClassInstanceParser "" code) of
             Left  _ -> Error
             Right x -> x)

testNewClassInstanceParserNoArgsNoParens :: Test
testNewClassInstanceParserNoArgsNoParens = do
    let code = "new ClassName"
    TestCase $ assertEqual code
        Error
        (case (parse newClassInstanceParser "" code) of
             Left  _ -> Error
             Right x -> x)

testNewClassInstanceParserSingleArg :: Test
testNewClassInstanceParserSingleArg = do
    let code = "new ClassName(1000)"
    TestCase $ assertEqual code
        (NewClassInstance (Identifier "ClassName") [Argument (ArithExpr (IntConst 1000))])
        (case (parse newClassInstanceParser "" code) of
             Left  _ -> Error
             Right x -> x)

testNewClassInstanceParserMultiArgs :: Test
testNewClassInstanceParserMultiArgs = do
    let code = "new ClassName(1000, 10 new OtherClassName(varName, otherVarName)))"
    TestCase $ assertEqual code
        Error
        (case (parse newClassInstanceParser "" code) of
             Left  _ -> Error
             Right x -> x)

testNewClassInstanceParserMissingNew :: Test
testNewClassInstanceParserMissingNew = do
    let code = "ClassName()"
    TestCase $ assertEqual code
        Error
        (case (parse newClassInstanceParser "" code) of
             Left  _ -> Error
             Right x -> x)

testNewClassInstanceParserMissingLeftParen :: Test
testNewClassInstanceParserMissingLeftParen = do
    let code = "new ClassName)"
    TestCase $ assertEqual code
        Error
        (case (parse newClassInstanceParser "" code) of
             Left  _ -> Error
             Right x -> x)

testNewClassInstanceParserMissingRightParen :: Test
testNewClassInstanceParserMissingRightParen = do
    let code = "new ClassName("
    TestCase $ assertEqual code
        Error
        (case (parse newClassInstanceParser "" code) of
             Left  _ -> Error
             Right x -> x)
