module Parser.IfElseStatementParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import Parser.ExprParser

-- Individual if, elif, and else parser tests
testIfStmtParserBooleanTrue :: Test
testIfStmtParserBooleanTrue = do
    let code = unlines [ "if(True)"
                       , "  println(\"Test\")"
                       ]
    TestCase $ assertEqual code
        (If (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")])
        (case (parse (ifStmtParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testIfStmtParserBooleanFalse :: Test
testIfStmtParserBooleanFalse = do
    let code = unlines [ "if(True)"
                       , "  println(\"Test\")"
                       ]
    TestCase $ assertEqual code
        (If (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")])
        (case (parse (ifStmtParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testIfStmtParserObjectVar :: Test
testIfStmtParserObjectVar = do
    let code = unlines [ "if(True and objName.varName)"
                       , "  println(\"Test\")"
                       ]
    TestCase $ assertEqual code
        (If (BooleanExpr (BBinary And (BoolConst True) (ClassVariable "objName" "varName"))) [Print (StringLiteral "Test")])
        (case (parse (ifStmtParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testIfStmtParserAnd :: Test
testIfStmtParserAnd = do
    let code = unlines [ "if(True and objName.varName and varName)"
                       , "  println(\"Test\")"
                       ]
    TestCase $ assertEqual code
        (If (BooleanExpr (BBinary And (BBinary And (BoolConst True) (ClassVariable "objName" "varName"))(Identifier "varName"))) [Print (StringLiteral "Test")])
        (case (parse (ifStmtParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testIfStmtParserOr :: Test
testIfStmtParserOr = do
    let code = unlines [ "if(True or objName.varName)"
                       , "  println(\"Test\")"
                       ]
    TestCase $ assertEqual code
        (If (BooleanExpr (BBinary Or (BoolConst True) (ClassVariable "objName" "varName"))) [Print (StringLiteral "Test")])
        (case (parse (ifStmtParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testIfStmtParserAndOr :: Test
testIfStmtParserAndOr = do
    let code = unlines [ "if(True and varName or objName.varName)"
                       , "  println(\"Test\")"
                       ]
    TestCase $ assertEqual code
        (If (BooleanExpr (BBinary Or (BBinary And (BoolConst True) (Identifier "varName")) (ClassVariable "objName" "varName"))) [Print (StringLiteral "Test")])
        (case (parse (ifStmtParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testIfStmtParserElif :: Test
testIfStmtParserElif = do
    let code = unlines [ "elif(True)"
                       , "  println(\"Test\")"
                       ]
    TestCase $ assertEqual code
        (ElseIf (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")])
        (case (parse (elifStmtParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testIfStmtParserElse :: Test
testIfStmtParserElse = do
    let code = unlines [ "else"
                       , "  println(\"Test\")"
                       ]
    TestCase $ assertEqual code
        (Else [Print (StringLiteral "Test")])
        (case (parse (elseStmtParser) "" code) of
             Left  _ -> Error
             Right x -> x)

-- If statement block parser tests
testIfStmtParserBlockIf :: Test
testIfStmtParserBlockIf = do
    let code = unlines [ "if(True)"
                       , "  println(\"Test\")"
                       ]
    let test1 = TestCase $ assertEqual code
                    (IfStatement (If (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")]) Nothing Nothing)
                    (case (parse (ifStatementBlockParser) "" code) of
                         Left  _ -> Error
                         Right x -> x)
    let test2 = TestCase $ assertEqual code
                    (IfStatement (If (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")]) Nothing Nothing)
                    (case (parse (expr') "" code) of
                         Left  _ -> Error
                         Right x -> x)
    TestList[test1, test2]

testIfStmtParserBlockElif :: Test
testIfStmtParserBlockElif = do
    let code = unlines [ "if(True)"
                       , "  println(\"Test\")"
                       , "elif(True)"
                       , "  println(\"Test\")"
                       ]
    let test1 = TestCase $ assertEqual code
                    (IfStatement (If (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")]) (Just (ElseIf (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")])) Nothing)
                    (case (parse (ifStatementBlockParser) "" code) of
                         Left  _ -> Error
                         Right x -> x)
    let test2 = TestCase $ assertEqual code
                    (IfStatement (If (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")]) (Just (ElseIf (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")])) Nothing)
                    (case (parse (expr') "" code) of
                         Left  _ -> Error
                         Right x -> x)
    TestList[test1, test2]

testIfStmtParserBlockElifElse :: Test
testIfStmtParserBlockElifElse = do
    let code = unlines [ "if(True)"
                       , "  println(\"Test\")"
                       , "elif(True)"
                       , "  println(\"Test\")"
                       , "else"
                       , "  println(\"Test\")"
                       ]
    let test1 = TestCase $ assertEqual code
                    (IfStatement (If (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")]) (Just (ElseIf (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")])) (Just (Else [Print (StringLiteral "Test")])))
                    (case (parse (ifStatementBlockParser) "" code) of
                         Left  _ -> Error
                         Right x -> x)
    let test2 = TestCase $ assertEqual code
                    (IfStatement (If (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")]) (Just (ElseIf (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")])) (Just (Else [Print (StringLiteral "Test")])))
                    (case (parse (expr') "" code) of
                         Left  _ -> Error
                         Right x -> x)
    TestList[test1, test2]

testIfStmtParserBlockElse :: Test
testIfStmtParserBlockElse = do
    let code = unlines [ "if(True)"
                       , "  println(\"Test\")"
                       , "else"
                       , "  println(\"Test\")"
                       ]
    let test1 = TestCase $ assertEqual code
                    (IfStatement (If (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")]) Nothing (Just (Else [Print (StringLiteral "Test")])))
                    (case (parse (ifStatementBlockParser) "" code) of
                         Left  _ -> Error
                         Right x -> x)
    let test2 = TestCase $ assertEqual code
                    (IfStatement (If (BooleanExpr (BoolConst True)) [Print (StringLiteral "Test")]) Nothing (Just (Else [Print (StringLiteral "Test")])))
                    (case (parse (expr') "" code) of
                         Left  _ -> Error
                         Right x -> x)
    TestList[test1, test2]