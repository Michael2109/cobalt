module Parser.ReassignParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testReassignParser :: Test
testReassignParser = do
    let code1 = "x <- new ClassName(10)"
    let test1 = testParseSuccess code1 (Reassign (Name "x") (ExprAssignment (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [IntConst 10]) Nothing))) statementParser

    let codeArithmetic = "varName <- 100 - y"
    let testArithmetic = testParseSuccess codeArithmetic (Reassign (Name "varName") (ExprAssignment (ABinary Subtract (IntConst 100) (Identifier $ Name "y")))) statementParser

    let codeArithmeticTwoVars = "x <- x + direction"
    let testArithmeticTwoVars = testParseSuccess codeArithmeticTwoVars (Reassign (Name "x") (ExprAssignment (ABinary Add (Identifier (Name "x")) (Identifier (Name "direction"))))) statementParser

    let codeClassVar = "Var_Name <- ClassName.VarName"
    let testClassVar = testParseSuccess codeClassVar (Reassign (Name "Var_Name") (ExprAssignment (BlockExpr [Identifier (Name "ClassName"),Identifier (Name "VarName")]))) statementParser

    TestList [ test1
             , testArithmetic
             , testArithmeticTwoVars
             , testClassVar
             ]
