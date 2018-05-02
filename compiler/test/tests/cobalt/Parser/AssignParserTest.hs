module Parser.AssignParserTest where

import Test.HUnit
import Text.Megaparsec

import TestUtil.ParserTestUtil
import AST.AST
import Parser.ExprParser

testAssignParser :: Test
testAssignParser = do
    let codeInlineNoType = "let x = y"
    let testInlineNoType = testParseSuccess codeInlineNoType (Assign (Name "x") Nothing (ExprAssignment $ (Identifier (Name "y")))) assignParser

    let codeInline = "let x: Int = y"
    let testInline = testParseSuccess codeInline (Assign (Name "x") (Just (TypeRef (RefLocal (Name "Int")))) (ExprAssignment $ (Identifier (Name "y")))) assignParser

    let codeDoBlock = unlines [ "let x: Int = do"
                              , "    x"
                              , "    y"
                              ]
    let testDoBlock = testParseSuccess codeDoBlock (Assign (Name "x") (Just (TypeRef (RefLocal (Name "Int")))) (StmtAssignment $ BlockStmt [ExprAsStmt (Identifier (Name "x")),ExprAsStmt (Identifier (Name "y"))])) assignParser

    TestList [testInlineNoType, testInline, testDoBlock]

testAssignParserMultiple :: Test
testAssignParserMultiple = do
    let codeInlineNoType = "let x,y = z"
    let testInlineNoType = testParseSuccess codeInlineNoType (AssignMultiple [Name "x",Name "y"] Nothing (ExprAssignment (Identifier (Name "z")))) assignParser


    let codeInline = "let x,y: Int = z"
    let testInline = testParseSuccess codeInline (AssignMultiple [Name "x",Name "y"] (Just (TypeRef (RefLocal (Name "Int")))) (ExprAssignment (Identifier (Name "z")))) assignParser

    let codeDoBlockNoType = unlines [ "let x,y = do"
                              , "    i"
                              , "    j"
                              ]
    let testDoBlockNoType = testParseSuccess codeDoBlockNoType (AssignMultiple [Name "x",Name "y"] Nothing (StmtAssignment $ BlockStmt [ExprAsStmt (Identifier (Name "i")),ExprAsStmt (Identifier (Name "j"))])) assignParser

    let codeDoBlock = unlines [ "let x,y: Int = do"
                              , "    i"
                              , "    j"
                              ]
    let testDoBlock = testParseSuccess codeDoBlock (AssignMultiple [Name "x",Name "y"] (Just (TypeRef (RefLocal (Name "Int")))) (StmtAssignment $ BlockStmt [ExprAsStmt (Identifier (Name "i")),ExprAsStmt (Identifier (Name "j"))])) assignParser


    TestList [testInlineNoType, testInline, testDoBlock, testDoBlockNoType]
