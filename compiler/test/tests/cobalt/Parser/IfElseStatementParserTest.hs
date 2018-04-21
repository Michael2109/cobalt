module Parser.IfElseStatementParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testIfStmtParserBooleanTrue :: Test
testIfStmtParserBooleanTrue = do
    let code = "if(True)"
    TestCase $ assertEqual code
        (If (IfStatement (BoolConst True) (Block [])) Nothing Nothing)
        (case (parse (ifStatementParser) "" code) of
             Left  e -> error (show e)
             Right x -> x)

testIfStmtParserBooleanFalse :: Test
testIfStmtParserBooleanFalse = do
    let code = "if(False)"
    TestCase $ assertEqual code
        (If (IfStatement (BoolConst False) (Block [])) Nothing Nothing)
        (case (parse (ifStatementParser) "" code) of
             Left  e -> error (show e)
             Right x -> x)

testIfStmtParserElifTrue :: Test
testIfStmtParserElifTrue = do
    let code = unlines [ "if(True)"
                       , "  i"
                       , "elif(True)"
                       , "  j"
                       ]
    TestCase $ assertEqual code
        (If (IfStatement (BoolConst True) (Block [Identifier (Name "i")])) (Just (ElifStatement (BoolConst True) (Block [Identifier (Name "j")]))) Nothing)
        (case (parse (ifStatementParser) "" code) of
             Left  e -> error (show e)
             Right x -> x)

testIfStmtParserElifFalse :: Test
testIfStmtParserElifFalse = do
    let code = unlines [ "if(True)"
                       , "  i"
                       , "elif(False)"
                       , "  j"
                       ]
    TestCase $ assertEqual code
        (If (IfStatement (BoolConst True) (Block [Identifier (Name "i")])) (Just (ElifStatement (BoolConst False) (Block [Identifier (Name "j")]))) Nothing)
        (case (parse (ifStatementParser) "" code) of
             Left  e -> error (show e)
             Right x -> x)

testIfStmtParserElifElse :: Test
testIfStmtParserElifElse = do
    let code = unlines [ "if(True)"
                       , "  i"
                       , "elif(True)"
                       , "  j"
                       , "else"
                       , "  k"
                       ]
    TestCase $ assertEqual code
        (If (IfStatement (BoolConst True) (Block [Identifier (Name "i")])) (Just (ElifStatement (BoolConst True) (Block [Identifier (Name "j")]))) (Just (ElseStatement (Block [Identifier (Name "k")]))))
        (case (parse (ifStatementParser) "" code) of
             Left  e -> error (show e)
             Right x -> x)

testIfStmtParserElse :: Test
testIfStmtParserElse = do
    let code = unlines [ "if(True)"
                       , "  i"
                       , "else"
                       , "  k"
                       ]
    TestCase $ assertEqual code
        (If (IfStatement (BoolConst True) (Block [Identifier (Name "i")])) Nothing (Just (ElseStatement (Block [Identifier (Name "k")]))))
        (case (parse (ifStatementParser) "" code) of
             Left  e -> error (show e)
             Right x -> x)
