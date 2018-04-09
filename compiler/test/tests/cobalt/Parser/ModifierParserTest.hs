module Parser.ModifierParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import AST.Modifier
import Parser.ExprParser

testAccessModifierParserPublic :: Test
testAccessModifierParserPublic = do
    let code = "private"
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Public] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (Argument (StringLiteral "Hello world"))])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testAccessModifierParserProtected :: Test
testAccessModifierParserProtected = do
    let code = "private"
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Public] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (Argument (StringLiteral "Hello world"))])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testAccessModifierParserPrivate :: Test
testAccessModifierParserPrivate = do
    let code = "private"
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Public] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (Argument (StringLiteral "Hello world"))])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testAbstractModifierParser :: Test
testAbstractModifierParser = do
    let code = "abstract"
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Public] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (Argument (StringLiteral "Hello world"))])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testFinalModifierParser :: Test
testFinalModifierParser = do
    let code = "final"
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Public] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (Argument (StringLiteral "Hello world"))])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)