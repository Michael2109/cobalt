module Parser.ParenthesesParserTest where

import Test.HUnit
import Text.Megaparsec


import Parser.BaseParser
import Parser.ExprParser
{-

testParenthesesParserVar :: Test
testParenthesesParserVar = do
    let code = "(x)"
    TestCase $ assertEqual code
        (Identifier "x")
        (case (parse (parens aExpr) "" code) of
             Left  _ -> Error
             Right x -> x)

testParenthesesParserNested :: Test
testParenthesesParserNested = do
    let code = "(((((x)))))"
    TestCase $ assertEqual code
        (Identifier "x")
        (case (parse (parens aExpr) "" code) of
             Left  _ -> Error
             Right x -> x)

testParenthesesParserNoOpenFail :: Test
testParenthesesParserNoOpenFail = do
    let code = "x)"
    TestCase $ assertEqual code
        Error
        (case (parse (parens aExpr) "" code) of
             Left  _ -> Error
             Right x -> x)

testParenthesesParserNoCloseFail :: Test
testParenthesesParserNoCloseFail = do
    let code = "(x"
    TestCase $ assertEqual code
        Error
        (case (parse (parens aExpr) "" code) of
             Left  _ -> Error
             Right x -> x)
-}
