module Parser.ForLoopParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testForLoopGeneratorParser :: Test
testForLoopGeneratorParser = do
    let code = unlines [ "for(i <- 0 to 10)"
                       , "  i"
                       ]
    let test = testParseSuccess code (For (Identifier (Name "i")) (IntConst 0) (IntConst 10) (BlockStmt [ExprAsStmt (Identifier (Name "i"))])) forLoopGeneratorParser

    TestList [test]
