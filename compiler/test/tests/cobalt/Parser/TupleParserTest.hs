module Parser.TupleParserTest where

import Test.HUnit

import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testTupleParser :: Test
testTupleParser = do
    let code = "x, y, z"
    TestCase $ assertEqual code
        (Tuple [Identifier (Name "x"),Identifier (Name "y"),Identifier (Name "z")])
        (case (parse tupleParser "" code) of
             Left  e -> error $ show e
             Right x -> x)
