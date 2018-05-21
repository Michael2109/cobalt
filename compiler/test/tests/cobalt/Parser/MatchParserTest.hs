module Parser.MatchParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testMatchParser :: Test
testMatchParser = do
    let codeCase = "ClassName1 -> i"
    let testCase = testParseSuccess codeCase (Case (Identifier (Name "ClassName1")) (Inline (Identifier (Name "i")))) caseParser

    let codeMatch = unlines [ "match obj with"
                            , "    ClassName1 -> i"
                            , "    ClassName2 -> j"
                            , "    (_)        -> k"
                            ]
    let testMatch = testParseSuccess codeMatch (Match (Identifier (Name "obj")) [Case (Identifier (Name "ClassName1")) (Inline (Identifier (Name "i"))),Case (Identifier (Name "ClassName2")) (Inline (Identifier (Name "j"))),Case (Identifier (Name "_")) (Inline (Identifier (Name "k")))]) statementParser

    let codeMatchDoBlock = unlines [ "match obj with"
                                   , "    ClassName1 -> do"
                                   , "        i"
                                   , "        j"
                                   , "    ClassName2 -> j"
                                   , "    (_)        -> do"
                                   , "        k"
                                   , "        z"
                                   ]
    let testMatchDoBlock = testParseSuccess codeMatchDoBlock (Match (Identifier (Name "obj")) [Case (Identifier (Name "ClassName1")) (DoBlock (BlockStmt [ExprAsStmt (Identifier (Name "i")),ExprAsStmt (Identifier (Name "j"))])),Case (Identifier (Name "ClassName2")) (Inline (Identifier (Name "j"))),Case (Identifier (Name "_")) (DoBlock (BlockStmt [ExprAsStmt (Identifier (Name "k")),ExprAsStmt (Identifier (Name "z"))]))]) statementParser

    TestList [ testCase
             , testMatch
             , testMatchDoBlock
             ]
