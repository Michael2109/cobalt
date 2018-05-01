module Parser.ForLoopParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testForLoopGeneratorParser :: Test
testForLoopGeneratorParser = do
    let code = unlines [ "for(i <- 0 to 10)"
                       , "  i"
                       ]
    TestCase $ assertEqual code
        (For (Identifier (Name "i")) (IntConst 0) (IntConst 10) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]))
        (case (parse (forLoopGeneratorParser) "" code) of
             Left  e -> error $ show e
             Right x -> x)
