module Parser.ReassignParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testReassignParser :: Test
testReassignParser = do
    let code = "x <- new ClassName(10)"
    let test = TestCase $ assertEqual code
                   (Reassign (Name "x") (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [AExprContainer (IntConst 10)]) Nothing))
                   (case (parse reassignParser "" code) of
                       Left  e -> error $ show e
                       Right x -> x)

    let codeArithmetic = "varName <- 100 - y"
    let testArithmetic = TestCase $ assertEqual codeArithmetic
                   (Reassign (Name "x") (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [AExprContainer (IntConst 10)]) Nothing))
                   (case (parse reassignParser "" code) of
                       Left  e -> error $ show e
                       Right x -> x)

    let codeArithmeticTwoVars = "x <- x + direction"
    let testArithmeticTwoVars = TestCase $ assertEqual codeArithmeticTwoVars
                   (Reassign (Name "x") (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [AExprContainer (IntConst 10)]) Nothing))
                   (case (parse reassignParser "" code) of
                       Left  e -> error $ show e
                       Right x -> x)

    let codeClassVar = "Var_Name <- ClassName.VarName"
    let testClassVar = TestCase $ assertEqual codeClassVar
                   (Reassign (Name "x") (NewClassInstance (TypeRef (RefLocal (Name "ClassName"))) (BlockExpr [AExprContainer (IntConst 10)]) Nothing))
                   (case (parse reassignParser "" code) of
                       Left  e -> error $ show e
                       Right x -> x)

    TestList [test, testArithmetic, testArithmeticTwoVars, testClassVar]
