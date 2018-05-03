module TestUtil.ParserTestUtil where

import Test.HUnit
import Text.Megaparsec
import Text.Pretty.Simple
import PrettyError
import Data.Monoid

import AST.AST
import Parser.ExprParser

testParseSuccess code result parser = TestCase $ assertEqual code
    result
    (case (parse parser "" code) of
        Left  e -> prettyError (code, e)
        Right x -> x)
