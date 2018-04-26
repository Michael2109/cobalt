module Parser.TernaryParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testTernaryParser :: Test
testTernaryParser = do

    let codeTernaryParenthesis = "if (True) then i else j"
    let testTernaryParenthesis = TestCase $ assertEqual codeTernaryParenthesis
                           (Ternary (BoolConst True) (Identifier (Name "i")) (Identifier (Name "j")))
                           (case (parse (ternaryParser) "" codeTernaryParenthesis) of
                               Left  e -> error (show e)
                               Right x -> x)

    let codeTernaryNoParenthesis = "if True then i else j"
    let testTernaryNoParenthesis = TestCase $ assertEqual codeTernaryNoParenthesis
                       (Ternary (BoolConst True) (Identifier (Name "i")) (Identifier (Name "j")))
                       (case (parse (ternaryParser) "" codeTernaryNoParenthesis) of
                           Left  e -> error (show e)
                           Right x -> x)

    TestList [testTernaryParenthesis, testTernaryNoParenthesis]
