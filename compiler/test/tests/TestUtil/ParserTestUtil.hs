module TestUtil.ParserTestUtil where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testParseSuccess code result parser = TestCase $ assertEqual code
    result
    (case (parse parser "" code) of
        Left  e -> error $ code ++ " - " ++ show e
        Right x -> x)
