module Parser.RExprParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parser.ExprParser

testRExprParserGreaterVar :: Test
testRExprParserGreaterVar = do
    let code = "x > y"
    TestCase $ assertEqual code
        (RBinary Greater (Identifier "x") (Identifier "y"))
        (case (parse rExpr "" code) of
             Left  _ -> Error
             Right x -> x)

testRExprParserLessVar :: Test
testRExprParserLessVar = do
    let code = "x < y"
    TestCase $ assertEqual code
        (RBinary Less (Identifier "x") (Identifier "y"))
        (case (parse rExpr "" code) of
             Left  _ -> Error
             Right x -> x)

testRExprParserGreaterEqualVar :: Test
testRExprParserGreaterEqualVar = do
    let code = "x >= y"
    TestCase $ assertEqual code
        (RBinary GreaterEqual (Identifier "x") (Identifier "y"))
        (case (parse rExpr "" code) of
             Left  _ -> Error
             Right x -> x)

testRExprParserLessEqualVar :: Test
testRExprParserLessEqualVar = do
    let code = "x <= y"
    TestCase $ assertEqual code
        (RBinary LessEqual (Identifier "x") (Identifier "y"))
        (case (parse rExpr "" code) of
             Left  _ -> Error
             Right x -> x)

testRExprParserGreaterInt :: Test
testRExprParserGreaterInt = do
    let code = "100 > 200"
    TestCase $ assertEqual code
        (RBinary Greater (IntConst 100) (IntConst 200))
        (case (parse rExpr "" code) of
             Left  _ -> Error
             Right x -> x)

testRExprParserLessInt :: Test
testRExprParserLessInt = do
    let code = "100 < 200"
    TestCase $ assertEqual code
        (RBinary Less (IntConst 100) (IntConst 200))
        (case (parse rExpr "" code) of
             Left  _ -> Error
             Right x -> x)

testRExprParserGreaterEqualInt :: Test
testRExprParserGreaterEqualInt = do
    let code = "100 >= 200"
    TestCase $ assertEqual code
        (RBinary GreaterEqual (IntConst 100) (IntConst 200))
        (case (parse rExpr "" code) of
             Left  _ -> Error
             Right x -> x)

testRExprParserLessEqualInt :: Test
testRExprParserLessEqualInt = do
    let code = "100 <= 200"
    TestCase $ assertEqual code
        (RBinary LessEqual (IntConst 100) (IntConst 200))
        (case (parse rExpr "" code) of
             Left  _ -> Error
             Right x -> x)
