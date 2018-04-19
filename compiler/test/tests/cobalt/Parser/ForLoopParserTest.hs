module Parser.ForLoopParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parser.ExprParser

testForLoopParser :: Test
testForLoopParser = do
    let code = unlines [ "for(i <- 0 to 10)"
                       , "  println(i)"
                       ]
    TestCase $ assertEqual code
        (For "i" (IntConst 0) (IntConst 10) [Print $ Identifier "i"])
        (case (parse (forLoopParser) "" code) of
             Left  _ -> Error
             Right x -> x)
