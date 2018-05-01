module Parser.IfStatementParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.AST
import Parser.ExprParser

testIfStmtParser :: Test
testIfStmtParser = do

    let codeTrue = unlines [ "if(True) then"
                           , "    x"
                           ]
    let testTrue = TestCase $ assertEqual codeTrue
                       (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "x"))]) Nothing)
                       (case (parse (ifStatementParser) "" codeTrue) of
                           Left  e -> error (show e)
                           Right x -> x)

    let codeFalse = unlines [ "if(False) then"
                            , "    x"
                            ]
    let testFalse = TestCase $ assertEqual codeFalse
                        (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "x"))]) Nothing)
                        (case (parse (ifStatementParser) "" codeFalse) of
                           Left  e -> error (show e)
                           Right x -> x)


    let codeElifTrue = unlines [ "if(True) then"
                               , "  i"
                               , "elif(True) then"
                               , "  j"
                               ]
    let testElifTrue = TestCase $ assertEqual codeElifTrue
                           (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "j"))]) Nothing)))
                           (case (parse (ifStatementParser) "" codeElifTrue) of
                               Left  e -> error (show e)
                               Right x -> x)

    let codeElifFalse = unlines [ "if(False) then"
                                , "  i"
                                , "elif(False) then"
                                , "  j"
                                ]
    let testElifFalse = TestCase $ assertEqual codeElifFalse
                            (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "j"))]) Nothing)))
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
                           (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "j"))]) (Just (BlockStmt [ExprAsStmt (Identifier (Name "k"))])))))
                           (case (parse (ifStatementParser) "" codeElifElse) of
                               Left  e -> error (show e)
                               Right x -> x)

    let codeElse = unlines [ "if(True) then"
                           , "  i"
                           , "else"
                           , "  k"
                           ]
    let testElse = TestCase $ assertEqual codeElse
                       (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (BlockStmt [ExprAsStmt (Identifier (Name "k"))])))
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
    let testMultipleElifsFinishedWithElse = TestCase $ assertEqual codeMultipleElifsFinishedWithElse
                       (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "x"))]) (Just (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "i"))]) (Just (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "f"))]) (Just (BlockStmt [ExprAsStmt (Identifier (Name "l"))])))))))
                       (case (parse (ifStatementParser) "" codeMultipleElifsFinishedWithElse) of
                           Left  e -> error (show e)
                           Right x -> x)


    let codeMultipleElifsWithoutElse = unlines [ "if(True) then"
                                               , "    x"
                                               , "elif(True) then"
                                               , "    y"
                                               , "elif(False) then"
                                               , "    z"
                                               ]
    let testMultipleElifsWithoutElse = TestCase $ assertEqual codeMultipleElifsWithoutElse
                       (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "x"))]) (Just (If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "y"))]) (Just (If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "z"))]) Nothing)))))
                       (case (parse (ifStatementParser) "" codeMultipleElifsWithoutElse) of
                           Left  e -> error (show e)
                           Right x -> x)

    let codeNestedWithoutElseNoParentheses = unlines [ "if (True) then"
                                                     , "    if (False) then "
                                                     , "        k"
                                                     , "    if True then"
                                                     , "        j"
                                                     , "    else"
                                                     , "        m"
                                                     ]
    let testNestedWithoutElseNoParentheses = TestCase $ assertEqual codeNestedWithoutElseNoParentheses
                       (If (BoolConst True) (BlockStmt [If (BoolConst False) (BlockStmt [ExprAsStmt (Identifier (Name "k"))]) Nothing,If (BoolConst True) (BlockStmt [ExprAsStmt (Identifier (Name "j"))]) (Just (BlockStmt [ExprAsStmt (Identifier (Name "m"))]))]) Nothing)
                       (case (parse (ifStatementParser) "" codeNestedWithoutElseNoParentheses) of
                           Left  e -> error (show e)
                           Right x -> x)

    TestList [ testTrue
             , testFalse
             , testElifTrue
             , testElifFalse
             , testElifElse
             , testElse
             , testMultipleElifsFinishedWithElse
             , testMultipleElifsWithoutElse
             , testNestedWithoutElseNoParentheses
             ]
