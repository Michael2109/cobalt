module Parser.AExprParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

{--
testAExprParserVar :: Test
testAExprParserVar = do
    let code = "x"
    TestCase $ assertEqual code
        (Name "x")
        (case (parse (aExpr) "" code) of
             Left  e -> error $ show e
             Right x -> x)
--}

testAExprParserInt :: Test
testAExprParserInt = do
    let code1 = "1000"
    let test1 = TestCase $ assertEqual code1
                    (IntConst 1000)
                    (case (parse (aExpr) "" code1) of
                         Left  e -> error $ show e
                         Right x -> x)
    let code2 = "-1000"
    let test2 = TestCase $ assertEqual code2
                    (Neg (IntConst 1000))
                    (case (parse (aExpr) "" code2) of
                         Left  e -> error $ show e
                         Right x -> x)
    TestList [test1, test2]