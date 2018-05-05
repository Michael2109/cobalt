module Parser.ThisVarParserTest where

import Test.HUnit
import Text.Megaparsec


import Parser.Parser
{-

testThisVarParserStartsDigitFail :: Test
testThisVarParserStartsDigitFail = do
    let code = "this.123"
    TestCase $ assertEqual code
        Error
        (case (parse thisVarParser "" code) of
             Left  _ -> Error
             Right x -> x)

testThisVarParserContainsCapital :: Test
testThisVarParserContainsCapital = do
    let code = "this.ABC"
    TestCase $ assertEqual code
        (ThisVar $ Identifier "ABC")
        (case (parse thisVarParser "" code) of
             Left  _ -> Error
             Right x -> x)

testThisVarParserContainsDigit :: Test
testThisVarParserContainsDigit = do
    let code = "this.a1b2c3"
    TestCase $ assertEqual code
        (ThisVar $ Identifier "a1b2c3")
        (case (parse thisVarParser "" code) of
             Left  _ -> Error
             Right x -> x)

testThisVarParserContainsUnderscore :: Test
testThisVarParserContainsUnderscore = do
    let code = "this.my_var"
    TestCase $ assertEqual code
        (ThisVar $ Identifier "my_var")
        (case (parse thisVarParser "" code) of
             Left  _ -> Error
             Right x -> x)

testThisVarParserNotThisFail :: Test
testThisVarParserNotThisFail = do
    let code = "thi.stypo"
    TestCase $ assertEqual code
        Error
        (case (parse thisVarParser "" code) of
             Left  _ -> Error
             Right x -> x)
-}
