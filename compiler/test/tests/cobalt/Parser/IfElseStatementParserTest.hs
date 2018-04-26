module Parser.IfElseStatementParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testIfStmtParser :: Test
testIfStmtParser = do
{-
    let codeTrue = unlines [ "if(True) then"
                           , "    x"
                           ]
    let testTrue = TestCase $ assertEqual codeTrue
                       (If (IfStatement (BoolConst True) (Block [Identifier (Name "x")])) Nothing)
                       (case (parse (ifStatementParser) "" codeTrue) of
                           Left  e -> error (show e)
                           Right x -> x)

    let codeFalse = unlines [ "if(False) then"
                            , "    x"
                            ]
    let testFalse = TestCase $ assertEqual codeFalse
                        (If (IfStatement (BoolConst False) (Block [Identifier (Name "x")])) Nothing)
                        (case (parse (ifStatementParser) "" codeFalse) of
                           Left  e -> error (show e)
                           Right x -> x)


    let codeElifTrue = unlines [ "if(True) then"
                               , "  i"
                               , "elif(True) then"
                               , "  j"
                               ]
    let testElifTrue = TestCase $ assertEqual codeElifTrue
                           (If (IfStatement (BoolConst True) (Block [Identifier (Name "i")])) (Just (IfStatement (BoolConst True) (Block [Identifier (Name "j")]))))
                           (case (parse (ifStatementParser) "" codeElifTrue) of
                               Left  e -> error (show e)
                               Right x -> x)

    let codeElifFalse = unlines [ "if(False) then"
                                , "  i"
                                , "elif(False) then"
                                , "  j"
                                ]
    let testElifFalse = TestCase $ assertEqual codeElifFalse
                            (If (IfStatement (BoolConst False) (Block [Identifier (Name "i")])) (Just (IfStatement (BoolConst False) (Block [Identifier (Name "j")]))))
                            (case (parse (ifStatementParser) "" codeElifFalse) of
                                Left  e -> error (show e)
                                Right x -> x)

    let codeElifElse = unlines [ "if(True) then"
                               , "  i"
                               , "elif(True) then"
                               , "  j"
                               , "else"
                               , "  k"
                               ]
    let testElifElse = TestCase $ assertEqual codeElifElse
                           (If (IfStatement (BoolConst True) (Block [Identifier (Name "i")])) (Just (ElseStatement (Block [Identifier (Name "k")]))))
                           (case (parse (ifStatementParser) "" codeElifElse) of
                               Left  e -> error (show e)
                               Right x -> x)

    let codeElse = unlines [ "if(True) then"
                           , "  i"
                           , "else"
                           , "  k"
                           ]
    let testElse = TestCase $ assertEqual codeElse
                       (If (IfStatement (BoolConst True) (Block [Identifier (Name "i")])) (Just (ElseStatement (Block [Identifier (Name "k")]))))
                       (case (parse (ifStatementParser) "" codeElse) of
                           Left  e -> error (show e)
                           Right x -> x)

    let codeMultipleElifsFinishedWithElse = unlines [ "if(True) then"
                                                    , "    x"
                                                    , "elif(True) then"
                                                    , "    i"
                                                    , "elif(False) then"
                                                    , "    f"
                                                    , "else"
                                                    , "    l"
                                                    ]


    let testMultipleElifsWithoutElse = unlines [ "if(True) then"
                                               , "    x"
                                               , "elif(True) then"
                                               , "    y"
                                               , "elif(False) then"
                                               , "    z"
                                               ]


    let testNestedWithoutElseNoParentheses = unlines [ "if(True) then"
                                                     , "    if(False) then "
                                                     , "        k"
                                                     , "    if True then"
                                                     , "        j"
                                                     , "    else"
                                                     , "        m"
                                                     ]
-}


    TestList [{-testTrue, testFalse, testElifTrue, testElifFalse, testElifElse, testElse-}]
