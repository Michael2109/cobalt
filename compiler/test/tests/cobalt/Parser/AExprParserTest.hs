module Parser.AExprParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser


import AST.AST
import Parser.ExprParser

testAExprParser :: Test
testAExprParser = do
    let codeAdd = "x + 100"
    let testAdd = TestCase $ assertEqual codeAdd
                       (ABinary Add (Var "x") (IntConst 100))
                       (case (parse aExpr "" codeAdd) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeSubtract = "x - 100"
    let testSubtract = TestCase $ assertEqual codeSubtract
                       (ABinary Subtract (Var "x") (IntConst 100))
                       (case (parse aExpr "" codeSubtract) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeMultiply = "x * 100"
    let testMultiply = TestCase $ assertEqual codeMultiply
                       (ABinary Multiply (Var "x") (IntConst 100))
                       (case (parse aExpr "" codeMultiply) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeDivide = "x / 100"
    let testDivide = TestCase $ assertEqual codeDivide
                       (ABinary Divide (Var "x") (IntConst 100))
                       (case (parse aExpr "" codeDivide) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeMixed = "x / 100 * y + 200 - z"
    let testMixed = TestCase $ assertEqual codeMixed
                       (ABinary Subtract (ABinary Add (ABinary Multiply (ABinary Divide (Var "x") (IntConst 100)) (Var "y")) (IntConst 200)) (Var "z"))
                       (case (parse aExpr "" codeMixed) of
                           Left  e -> error $ show e
                           Right x -> x)

    let codeNegative = "- 100"
    let testNegative = TestCase $ assertEqual codeNegative
                       (Neg (IntConst 100))
                       (case (parse aExpr "" codeNegative) of
                           Left  e -> error $ show e
                           Right x -> x)

    TestList [ testAdd
             , testSubtract
             , testMultiply
             , testDivide
             , testMixed
             , testNegative
             ]
