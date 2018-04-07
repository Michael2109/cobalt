module Parsers.BooleanParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parsers.ExprParser

-- Boolean Expression tests
testBooleanParserTrue :: Test
testBooleanParserTrue = do
    let code = "True"
    TestCase $ assertEqual code
        (BooleanExpr (BoolConst True))
        (case (parse (booleanParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testBooleanParserFalse :: Test
testBooleanParserFalse = do
    let code = "False"
    TestCase $ assertEqual code
        (BooleanExpr (BoolConst False))
        (case (parse (booleanParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testBooleanParserIdentifier :: Test
testBooleanParserIdentifier = do
    let code = "true"
    TestCase $ assertEqual code
        (BooleanExpr (Identifier "true"))
        (case (parse (booleanParser) "" code) of
             Left  _ -> Error
             Right x -> x)

-- Less than

testBooleanParserLessThanVar :: Test
testBooleanParserLessThanVar = do
    let code = "x < y"
    TestCase $ assertEqual code
        (BooleanExpr (RBinary Less (Identifier "x") (Identifier "y")))
        (case (parse (booleanParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testBooleanParserLessThanInt :: Test
testBooleanParserLessThanInt = do
    let code = "100 < 300"
    TestCase $ assertEqual code
        (BooleanExpr (RBinary Less (IntConst 100) (IntConst 300)))
        (case (parse (booleanParser) "" code) of
             Left  _ -> Error
             Right x -> x)

-- Greater than

testBooleanParserGreaterThanVar :: Test
testBooleanParserGreaterThanVar = do
    let code = "x > y"
    TestCase $ assertEqual code
        (BooleanExpr (RBinary Greater (Identifier "x") (Identifier "y")))
        (case (parse (booleanParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testBooleanParserGreaterThanInt :: Test
testBooleanParserGreaterThanInt = do
    let code = "100 > 300"
    TestCase $ assertEqual code
        (BooleanExpr (RBinary Greater (IntConst 100) (IntConst 300)))
        (case (parse (booleanParser) "" code) of
             Left  _ -> Error
             Right x -> x)

-- Less than / equal to

testBooleanParserLessThanEqualVar :: Test
testBooleanParserLessThanEqualVar = do
    let code = "x <= y"
    TestCase $ assertEqual code
        (BooleanExpr (RBinary LessEqual (Identifier "x") (Identifier "y")))
        (case (parse (booleanParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testBooleanParserLessThanEqualInt :: Test
testBooleanParserLessThanEqualInt = do
    let code = "100 <= 300"
    TestCase $ assertEqual code
        (BooleanExpr (RBinary LessEqual (IntConst 100) (IntConst 300)))
        (case (parse (booleanParser) "" code) of
             Left  _ -> Error
             Right x -> x)

-- Greater than / equal to
testBooleanParserGreaterThanEqualVar :: Test
testBooleanParserGreaterThanEqualVar = do
    let code = "x >= y"
    TestCase $ assertEqual code
        (BooleanExpr (RBinary GreaterEqual (Identifier "x") (Identifier "y")))
        (case (parse (booleanParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testBooleanParserGreaterThanEqualInt :: Test
testBooleanParserGreaterThanEqualInt = do
    let code = "100 >= 300"
    TestCase $ assertEqual code
        (BooleanExpr (RBinary GreaterEqual (IntConst 100) (IntConst 300)))
        (case (parse (booleanParser) "" code) of
             Left  _ -> Error
             Right x -> x)
