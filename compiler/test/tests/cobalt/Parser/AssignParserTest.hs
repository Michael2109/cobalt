module Parser.AssignParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parser.ExprParser

{-
testAssignParserValWithType :: Test
testAssignParserValWithType = do
    let code = "let x: Int = 1"
    TestCase $ assertEqual code
        (Assign True (Just (Type (Identifier "Int"))) (Identifier "x") (IntConst 1))
        (case (parse assignParser "" code) of
             Left  _ -> Error
             Right x -> x)

testAssignParserValWithoutType :: Test
testAssignParserValWithoutType = do
    let code = "let x = 1"
    TestCase $ assertEqual code
        (Assign True Nothing (Identifier "x") (IntConst 1))
        (case (parse assignParser "" code) of
             Left  _ -> Error
             Right x -> x)

testAssignParserWithoutVal :: Test
testAssignParserWithoutVal = do
    let code = "x = 1"
    TestCase $ assertEqual code
        Error
        (case (parse assignParser "" code) of
             Left  _ -> Error
             Right x -> x)

testAssignParserVarWithType :: Test
testAssignParserVarWithType = do
    let code = "let mutable x: Int = 1"
    TestCase $ assertEqual code
        (Assign False (Just (Type (Identifier "Int"))) (Identifier "x") (IntConst 1))
        (case (parse assignParser "" code) of
             Left  _ -> Error
             Right x -> x)

testAssignParserVarWithoutType :: Test
testAssignParserVarWithoutType = do
    let code = "let mutable x = 1"
    TestCase $ assertEqual code
        (Assign False Nothing (Identifier "x") (IntConst 1))
        (case (parse assignParser "" code) of
             Left  _ -> Error
             Right x -> x)

testAssignParserValWithParameterizedType :: Test
testAssignParserValWithParameterizedType = do
    let code = "let x: Array[String] = 1"
    TestCase $ assertEqual code
        (Assign True (Just (Type (ParameterizedType (Identifier "Array") (TypeParameter (Identifier "String"))))) (Identifier "x") (IntConst 1))
        (case (parse assignParser "" code) of
             Left  _ -> Error
             Right x -> x)

testAssignParserVarWithParameterizedType :: Test
testAssignParserVarWithParameterizedType = do
    let code = "let mutable x: Array[String] = 1"
    TestCase $ assertEqual code
        (Assign False (Just (Type (ParameterizedType (Identifier "Array") (TypeParameter (Identifier "String"))))) (Identifier "x") (IntConst 1))
        (case (parse assignParser "" code) of
             Left  _ -> Error
             Right x -> x)

testAssignParserTwoVars :: Test
testAssignParserTwoVars = do
    let code = "let mutable x = x + y"
    TestCase $ assertEqual code
        (Assign False Nothing (Identifier "x") (ABinary Add (Identifier "x") (Identifier "y")))
        (case (parse assignParser "" code) of
             Left  _ -> Error
             Right x -> x)

testAssignParserThreeVars :: Test
testAssignParserThreeVars = do
    let code = "let mutable x = x + y - z"
    TestCase $ assertEqual code
        (Assign False Nothing (Identifier "x") (ABinary Subtract (ABinary Add (Identifier "x") (Identifier "y")) (Identifier "z")))
        (case (parse assignParser "" code) of
             Left  _ -> Error
             Right x -> x)

testAssignParserFourVars :: Test
testAssignParserFourVars = do
    let code = "let mutable x = x + y * q - z"
    TestCase $ assertEqual code
        (Assign False Nothing (Identifier "x") (ABinary Subtract (ABinary Add (Identifier "x") (ABinary Multiply (Identifier "y") (Identifier "q"))) (Identifier "z")))
        (case (parse assignParser "" code) of
             Left  _ -> Error
             Right x -> x)
-}
