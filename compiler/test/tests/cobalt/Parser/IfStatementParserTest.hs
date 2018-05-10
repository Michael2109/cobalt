module Parser.IfStatementParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testIfStmtParser :: Test
testIfStmtParser = do

    let codeTrue = unlines [ "if(True) then"
                           , "    x"
                           ]
    let testTrue = testParseSuccess codeTrue (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "x"))]) Nothing) ifStatementParser
    let testTrueStmt = testParseSuccess codeTrue (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "x"))]) Nothing) statementParser

    let codeFalse = unlines [ "if(False) then"
                            , "    x"
                            ]
    let testFalse = testParseSuccess codeFalse (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "x"))]) Nothing) ifStatementParser
    let testFalseStmt = testParseSuccess codeFalse (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "x"))]) Nothing) statementParser

    let codeElifTrue = unlines [ "if(True) then"
                               , "  i"
                               , "elif(True) then"
                               , "  j"
                               ]
    let testElifTrue = testParseSuccess codeElifTrue (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "j"))]) Nothing))) ifStatementParser
    let testElifTrueStmt = testParseSuccess codeElifTrue (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "j"))]) Nothing))) statementParser

    let codeElifFalse = unlines [ "if(False) then"
                                , "  i"
                                , "elif(False) then"
                                , "  j"
                                ]
    let testElifFalse = testParseSuccess codeElifFalse (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "j"))]) Nothing))) ifStatementParser
    let testElifFalseStmt = testParseSuccess codeElifFalse (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "j"))]) Nothing))) statementParser

    let codeElifElse = unlines [ "if(True) then"
                               , "  i"
                               , "elif(True) then"
                               , "  j"
                               , "else"
                               , "  k"
                               ]
    let testElifElse = testParseSuccess codeElifElse (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "j"))]) (Just (BlockStmt [ExprAsStmt (Identifier (Name "k"))]))))) ifStatementParser
    let testElifElseStmt = testParseSuccess codeElifElse (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "j"))]) (Just (BlockStmt [ExprAsStmt (Identifier (Name "k"))]))))) statementParser

    let codeElse = unlines [ "if(True) then"
                           , "  i"
                           , "else"
                           , "  k"
                           ]
    let testElse = testParseSuccess codeElse (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (BlockStmt [ExprAsStmt (Identifier (Name "k"))]))) ifStatementParser
    let testElseStmt = testParseSuccess codeElse (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (BlockStmt [ExprAsStmt (Identifier (Name "k"))]))) statementParser

    let codeMultipleElifsFinishedWithElse = unlines [ "if(True) then"
                                                    , "    x"
                                                    , "elif(True) then"
                                                    , "    i"
                                                    , "elif(False) then"
                                                    , "    f"
                                                    , "else"
                                                    , "    l"
                                                    ]
    let testMultipleElifsFinishedWithElse = testParseSuccess codeMultipleElifsFinishedWithElse (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "x"))]) (Just (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "f"))]) (Just (BlockStmt [ExprAsStmt (Identifier (Name "l"))]))))))) ifStatementParser
    let testMultipleElifsFinishedWithElseStmt = testParseSuccess codeMultipleElifsFinishedWithElse (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "x"))]) (Just (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "f"))]) (Just (BlockStmt [ExprAsStmt (Identifier (Name "l"))]))))))) statementParser

    let codeMultipleElifsWithoutElse = unlines [ "if(True) then"
                                               , "    x"
                                               , "elif(True) then"
                                               , "    y"
                                               , "elif(False) then"
                                               , "    z"
                                               ]
    let testMultipleElifsWithoutElse = testParseSuccess codeMultipleElifsWithoutElse (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "x"))]) (Just (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "y"))]) (Just (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "z"))]) Nothing))))) ifStatementParser
    let testMultipleElifsWithoutElseStmt = testParseSuccess codeMultipleElifsWithoutElse (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "x"))]) (Just (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "y"))]) (Just (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "z"))]) Nothing))))) statementParser

    let codeNestedWithoutElseNoParentheses = unlines [ "if (True) then"
                                                     , "    if (False) then "
                                                     , "        k"
                                                     , "    if True then"
                                                     , "        j"
                                                     , "    else"
                                                     , "        m"
                                                     ]
    let testNestedWithoutElseNoParentheses = testParseSuccess codeNestedWithoutElseNoParentheses (If (BoolConst True) (BlockStmt [If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "k"))]) Nothing,If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "j"))]) (Just (BlockStmt [ExprAsStmt (Identifier (Name "m"))]))]) Nothing) ifStatementParser
    let testNestedWithoutElseNoParenthesesStmt = testParseSuccess codeNestedWithoutElseNoParentheses (If (BoolConst True) (BlockStmt [If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "k"))]) Nothing,If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "j"))]) (Just (BlockStmt [ExprAsStmt (Identifier (Name "m"))]))]) Nothing) statementParser

    TestList [ testTrue
             , testTrueStmt
             , testFalse
             , testFalseStmt
             , testElifTrue
             , testElifTrueStmt
             , testElifFalse
             , testElifFalseStmt
             , testElifElse
             , testElifElseStmt
             , testElse
             , testElseStmt
             , testMultipleElifsFinishedWithElse
             , testMultipleElifsFinishedWithElseStmt
             , testMultipleElifsWithoutElse
             , testMultipleElifsWithoutElseStmt
             , testNestedWithoutElseNoParentheses
             , testNestedWithoutElseNoParenthesesStmt
             ]
