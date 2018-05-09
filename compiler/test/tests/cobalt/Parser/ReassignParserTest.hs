module Parser.ReassignParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testReassignParser :: Test
testReassignParser = do
    let code = "x <- new ClassName(10)"
    let test = testParseSuccess code (Reassign (Name "x") (ExprAssignment (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [AExprContainer (IntConst 10)]) Nothing))) reassignParser
    let testStmt = testParseSuccess code (Reassign (Name "x") (ExprAssignment (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [AExprContainer (IntConst 10)]) Nothing))) statementParser

    let codeArithmetic = "varName <- 100 - y"
    let testArithmetic = testParseSuccess codeArithmetic (Reassign (Name "varName") (ExprAssignment (AExprContainer (ABinary Subtract (IntConst 100) (Var "y"))))) reassignParser
    let testArithmeticStmt = testParseSuccess codeArithmetic (Reassign (Name "varName") (ExprAssignment (AExprContainer (ABinary Subtract (IntConst 100) (Var "y"))))) statementParser

    let codeArithmeticTwoVars = "x <- x + direction"
    let testArithmeticTwoVars = testParseSuccess codeArithmeticTwoVars (Reassign (Name "x") (ExprAssignment (Identifier (Name "x")))) reassignParser
    let testArithmeticTwoVarsStmt = testParseSuccess codeArithmeticTwoVars (Reassign (Name "x") (ExprAssignment (Identifier (Name "x")))) statementParser

    let codeClassVar = "Var_Name <- ClassName.VarName"
    let testClassVar = testParseSuccess codeClassVar (Reassign (Name "Var_Name") (ExprAssignment (BlockExpr [Identifier (Name "ClassName"),Identifier (Name "VarName")]))) reassignParser
    let testClassVarStmt = testParseSuccess codeClassVar (Reassign (Name "Var_Name") (ExprAssignment (BlockExpr [Identifier (Name "ClassName"),Identifier (Name "VarName")]))) statementParser

    TestList [ test
             , testStmt
             , testArithmetic
             , testArithmeticStmt
             , testArithmeticTwoVars
             , testArithmeticTwoVarsStmt
             , testClassVar
             , testClassVarStmt
             ]
