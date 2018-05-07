module Parser.AssignParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testAssignParser :: Test
testAssignParser = do
    let codeInlineNoType = "let x = y"
    let testInlineNoType = testParseSuccess codeInlineNoType (Assign (Name "x") Nothing (ExprAssignment $ (Identifier (Name "y")))) assignParser
    let testInlineNoTypeStmt = testParseSuccess codeInlineNoType (Assign (Name "x") Nothing (ExprAssignment $ (Identifier (Name "y")))) statementParser

    let codeInline = "let x: Int = y"
    let testInline = testParseSuccess codeInline (Assign (Name "x") (Just (TypeRef (RefLocal (Name "Int")))) (ExprAssignment $ (Identifier (Name "y")))) assignParser
    let testInlineStmt = testParseSuccess codeInline (Assign (Name "x") (Just (TypeRef (RefLocal (Name "Int")))) (ExprAssignment $ (Identifier (Name "y")))) statementParser

    let codeInlineValue = "let x = 10"
    let testInlineValue = testParseSuccess codeInlineValue (Assign (Name "x") Nothing (ExprAssignment (AExprContainer (IntConst 10)))) assignParser
    let testInlineValueStmt = testParseSuccess codeInlineValue (Assign (Name "x") Nothing (ExprAssignment (AExprContainer (IntConst 10)))) statementParser

    let codeDoBlock = unlines [ "let x: Int = do"
                              , "    x"
                              , "    y"
                              ]
    let testDoBlock = testParseSuccess codeDoBlock (Assign (Name "x") (Just (TypeRef (RefLocal (Name "Int")))) (StmtAssignment $ BlockStmt [ExprAsStmt (Identifier (Name "x")),ExprAsStmt (Identifier (Name "y"))])) assignParser
    let testDoBlockStmt = testParseSuccess codeDoBlock (Assign (Name "x") (Just (TypeRef (RefLocal (Name "Int")))) (StmtAssignment $ BlockStmt [ExprAsStmt (Identifier (Name "x")),ExprAsStmt (Identifier (Name "y"))])) statementParser

    TestList [testInlineNoType, testInlineNoTypeStmt, testInline, testInlineStmt, testInlineValue, testInlineValueStmt, testDoBlock, testDoBlockStmt]

testAssignParserMultiple :: Test
testAssignParserMultiple = do
    let codeInlineNoType = "let x,y = z"
    let testInlineNoType = testParseSuccess codeInlineNoType (AssignMultiple [Name "x",Name "y"] Nothing (ExprAssignment (Identifier (Name "z")))) assignParser
    let testInlineNoTypeStmt = testParseSuccess codeInlineNoType (AssignMultiple [Name "x",Name "y"] Nothing (ExprAssignment (Identifier (Name "z")))) statementParser

    let codeInline = "let x,y: Int = z"
    let testInline = testParseSuccess codeInline (AssignMultiple [Name "x",Name "y"] (Just (TypeRef (RefLocal (Name "Int")))) (ExprAssignment (Identifier (Name "z")))) assignParser
    let testInlineStmt = testParseSuccess codeInline (AssignMultiple [Name "x",Name "y"] (Just (TypeRef (RefLocal (Name "Int")))) (ExprAssignment (Identifier (Name "z")))) statementParser

    let codeDoBlockNoType = unlines [ "let x,y = do"
                              , "    i"
                              , "    j"
                              ]
    let testDoBlockNoType = testParseSuccess codeDoBlockNoType (AssignMultiple [Name "x",Name "y"] Nothing (StmtAssignment $ BlockStmt [ExprAsStmt (Identifier (Name "i")),ExprAsStmt (Identifier (Name "j"))])) assignParser
    let testDoBlockNoTypeStmt = testParseSuccess codeDoBlockNoType (AssignMultiple [Name "x",Name "y"] Nothing (StmtAssignment $ BlockStmt [ExprAsStmt (Identifier (Name "i")),ExprAsStmt (Identifier (Name "j"))])) statementParser

    let codeDoBlock = unlines [ "let x,y: Int = do"
                              , "    i"
                              , "    j"
                              ]
    let testDoBlock = testParseSuccess codeDoBlock (AssignMultiple [Name "x",Name "y"] (Just (TypeRef (RefLocal (Name "Int")))) (StmtAssignment $ BlockStmt [ExprAsStmt (Identifier (Name "i")),ExprAsStmt (Identifier (Name "j"))])) assignParser
    let testDoBlockStmt = testParseSuccess codeDoBlock (AssignMultiple [Name "x",Name "y"] (Just (TypeRef (RefLocal (Name "Int")))) (StmtAssignment $ BlockStmt [ExprAsStmt (Identifier (Name "i")),ExprAsStmt (Identifier (Name "j"))])) statementParser

    TestList [testInlineNoType, testInlineNoTypeStmt, testInline, testInlineStmt, testDoBlockNoType, testDoBlockNoTypeStmt, testDoBlock, testDoBlockStmt]
