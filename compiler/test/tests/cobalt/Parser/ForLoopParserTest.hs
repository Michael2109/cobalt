module Parser.ForLoopParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.ExprParser

testForLoopGeneratorParser :: Test
testForLoopGeneratorParser = do
    let code = unlines [ "for(i <- 0 to 10)"
                       , "  i"
                       ]
    let test = testParseSuccess code (For (Identifier (Name "i")) (AExprContainer (IntConst 0)) (AExprContainer (IntConst 10)) (BlockStmt [ExprAsStmt (Identifier (Name "i"))])) forLoopGeneratorParser

    TestList [test]
