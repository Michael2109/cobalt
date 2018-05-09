module TestUtil.ParserTestUtil where

import Test.HUnit
import Text.Megaparsec
import Data.Monoid

import AST.AST
import Parser.Parser

testParseSuccess code result parser = TestCase $ assertEqual code
    result
    (case (parse parser "" code) of
        Left  e -> error $ "(" ++ code ++ ") - " ++ show e
        Right x -> x)