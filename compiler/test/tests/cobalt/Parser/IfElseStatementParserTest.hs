module Parser.IfElseStatementParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testIfStmtParser :: Test
testIfStmtParser = do
    let codeTrue = unlines [ "if(True) then do"
                           , "    x"
                           ]
    let testTrue = TestCase $ assertEqual codeTrue
                       (If (IfStatement (BoolConst True) (Block [Identifier (Name "x")])) Nothing Nothing)
                       (case (parse (ifStatementParser) "" codeTrue) of
                           Left  e -> error (show e)
                           Right x -> x)

    let codeFalse = unlines [ "if(False) then do"
                            , "    x"
                            ]
    let testFalse = TestCase $ assertEqual codeFalse
                        (If (IfStatement (BoolConst False) (Block [Identifier (Name "x")])) Nothing Nothing)
                        (case (parse (ifStatementParser) "" codeFalse) of
                           Left  e -> error (show e)
                           Right x -> x)


    let codeElifTrue = unlines [ "if(True) then do"
                               , "  i"
                               , "elif(True) then do"
                               , "  j"
                               ]
    let testElifTrue = TestCase $ assertEqual codeElifTrue
                           (If (IfStatement (BoolConst True) (Block [Identifier (Name "i")])) (Just (ElifStatement (BoolConst True) (Block [Identifier (Name "j")]))) Nothing)
                           (case (parse (ifStatementParser) "" codeElifTrue) of
                               Left  e -> error (show e)
                               Right x -> x)

    let codeElifFalse = unlines [ "if(False) then do"
                                , "  i"
                                , "elif(False) then do"
                                , "  j"
                                ]
    let testElifFalse = TestCase $ assertEqual codeElifFalse
                            (If (IfStatement (BoolConst False) (Block [Identifier (Name "i")])) (Just (ElifStatement (BoolConst False) (Block [Identifier (Name "j")]))) Nothing)
                            (case (parse (ifStatementParser) "" codeElifFalse) of
                                Left  e -> error (show e)
                                Right x -> x)

    let codeElifElse = unlines [ "if(True) then do"
                               , "  i"
                               , "elif(True) then do"
                               , "  j"
                               , "else do"
                               , "  k"
                               ]
    let testElifElse = TestCase $ assertEqual codeElifElse
                           (If (IfStatement (BoolConst True) (Block [Identifier (Name "i")])) (Just (ElifStatement (BoolConst True) (Block [Identifier (Name "j")]))) (Just (ElseStatement (Block [Identifier (Name "k")]))))
                           (case (parse (ifStatementParser) "" codeElifElse) of
                               Left  e -> error (show e)
                               Right x -> x)

    let codeElse = unlines [ "if(True) then do"
                           , "  i"
                           , "else do"
                           , "  k"
                           ]
    let testElse = TestCase $ assertEqual codeElse
                       (If (IfStatement (BoolConst True) (Block [Identifier (Name "i")])) Nothing (Just (ElseStatement (Block [Identifier (Name "k")]))))
                       (case (parse (ifStatementParser) "" codeElse) of
                           Left  e -> error (show e)
                           Right x -> x)

    TestList [testTrue, testFalse, testElifTrue, testElifFalse, testElifElse, testElse]

testIfStmtParserInline :: Test
testIfStmtParserInline = do
    let codeTrue = "if(True) then x"
    let testTrue = TestCase $ assertEqual codeTrue
                       (If (IfStatement (BoolConst True) (Block [Identifier (Name "x")])) Nothing Nothing)
                       (case (parse (ifStatementParser) "" codeTrue) of
                           Left  e -> error (show e)
                           Right x -> x)

    let codeFalse = "if(False) then x"
    let testFalse = TestCase $ assertEqual codeFalse
                        (If (IfStatement (BoolConst False) (Block [Identifier (Name "x")])) Nothing Nothing)
                        (case (parse (ifStatementParser) "" codeFalse) of
                           Left  e -> error (show e)
                           Right x -> x)


    let codeElifTrue = unlines [ "if(True) then i"
                               , "elif(True) then j"
                               ]
    let testElifTrue = TestCase $ assertEqual codeElifTrue
                           (If (IfStatement (BoolConst True) (Block [Identifier (Name "i")])) (Just (ElifStatement (BoolConst True) (Block [Identifier (Name "j")]))) Nothing)
                           (case (parse (ifStatementParser) "" codeElifTrue) of
                               Left  e -> error (show e)
                               Right x -> x)

    let codeElifFalse = unlines [ "if(False) then i"
                                , "elif(False) then j"
                                ]
    let testElifFalse = TestCase $ assertEqual codeElifFalse
                            (If (IfStatement (BoolConst False) (Block [Identifier (Name "i")])) (Just (ElifStatement (BoolConst False) (Block [Identifier (Name "j")]))) Nothing)
                            (case (parse (ifStatementParser) "" codeElifFalse) of
                                Left  e -> error (show e)
                                Right x -> x)

    let codeElifElse = unlines [ "if(True) then i"
                               , "elif(True) then j"
                               , "else k"
                               ]
    let testElifElse = TestCase $ assertEqual codeElifElse
                           (If (IfStatement (BoolConst True) (Block [Identifier (Name "i")])) (Just (ElifStatement (BoolConst True) (Block [Identifier (Name "j")]))) (Just (ElseStatement (Block [Identifier (Name "k")]))))
                           (case (parse (ifStatementParser) "" codeElifElse) of
                               Left  e -> error (show e)
                               Right x -> x)

    let codeElse = unlines [ "if(True) then i"
                           , "else then k"
                           ]
    let testElse = TestCase $ assertEqual codeElse
                       (If (IfStatement (BoolConst True) (Block [Identifier (Name "i")])) Nothing (Just (ElseStatement (Block [Identifier (Name "k")]))))
                       (case (parse (ifStatementParser) "" codeElse) of
                           Left  e -> error (show e)
                           Right x -> x)

    TestList [testTrue, testFalse, testElifTrue, testElifFalse, testElifElse, testElse]