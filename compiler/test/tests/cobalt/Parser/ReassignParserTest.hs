module Parser.ReassignParserTest where

import Test.HUnit

import Text.Megaparsec

import AST.Block
import Parser.ExprParser

testReassignParserObject :: Test
testReassignParserObject = do
    let code = "x <- new ClassName(10)"
    TestCase $ assertEqual code
        (Reassign (Identifier "x") (NewClassInstance (Identifier "ClassName") [IntConst 10]))
        (case (parse reassignParser "" code) of
             Left  _ -> Error
             Right x -> x)

testReassignParserArithmetic :: Test
testReassignParserArithmetic = do
    let code = "varName <- 100 - y"
    TestCase $ assertEqual code
        (Reassign (Identifier "varName") (ABinary Subtract (IntConst 100) (Identifier "y")))
        (case (parse reassignParser "" code) of
             Left  _ -> Error
             Right x -> x)

testReassignParserArithmeticTwoVars :: Test
testReassignParserArithmeticTwoVars = do
    let code = "x <- x + direction"
    TestCase $ assertEqual code
        (Reassign (Identifier "x") (ABinary Add (Identifier "x") (Identifier "direction")))
        (case (parse reassignParser "" code) of
             Left  _ -> Error
             Right x -> x)

testReassignParserClassVar :: Test
testReassignParserClassVar = do
    let code = "Var_Name <- ClassName.VarName"
    TestCase $ assertEqual code
        (Reassign (Identifier "Var_Name") (ClassVariable "ClassName" "VarName"))
        (case (parse reassignParser "" code) of
             Left  _ -> Error
             Right x -> x)
