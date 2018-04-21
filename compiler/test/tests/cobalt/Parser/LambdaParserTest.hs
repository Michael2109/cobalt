module Parser.LambdaParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testLambdaParser :: Test
testLambdaParser = do

    let codeInline = "fun x -> x"
    let testInline = TestCase $ assertEqual codeInline
                    (Lambda (Identifier $ Name "x") (Block [Identifier $ Name "x"]))
                    (case (parse (lambdaParser) "" codeInline) of
                         Left  e -> error $ show e
                         Right x -> x)

    let codeDoBlock = unlines [ "fun x -> do"
                              , "    x"
                              , "    y"
                              , ""
                              ]
    let testDoBlock = TestCase $ assertEqual codeDoBlock
                    (Lambda (Identifier (Name "x")) (Block [Identifier (Name "x"),Identifier (Name "y")]))
                    (case (parse (lambdaParser) "" codeDoBlock) of
                         Left  e -> error $ show e
                         Right x -> x)
    TestList [testInline, testDoBlock]