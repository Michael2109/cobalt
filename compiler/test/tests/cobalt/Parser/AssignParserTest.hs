module Parser.AssignParserTest where

import Test.HUnit

import TestUtil.ParserTestUtil
import AST.AST
import Parser.Parser

testAssignParser :: Test
testAssignParser = do
    let codeInlineNoType = "let x = y"
    let testInlineNoType = testParseSuccess codeInlineNoType (Assign (Name "x") Nothing True (Inline $ (Identifier (Name "y")))) statementParser

    let codeInlineNoTypeMutable = "let mutable x = y"
    let testInlineNoTypeMutable = testParseSuccess codeInlineNoTypeMutable (Assign (Name "x") Nothing False (Inline $ (Identifier (Name "y")))) statementParser

    let codeInline = "let x: Int = y"
    let testInline = testParseSuccess codeInline (Assign (Name "x") (Just (TypeRef (RefLocal (Name "Int")))) True (Inline $ (Identifier (Name "y")))) statementParser

    let codeInlineValue = "let x = 10"
    let testInlineValue = testParseSuccess codeInlineValue (Assign (Name "x") Nothing True (Inline (IntConst 10))) statementParser

    let codeDoBlock = unlines [ "let x: Int = do"
                              , "    x"
                              , "    y"
                              ]
    let testDoBlock = testParseSuccess codeDoBlock (Assign (Name "x") (Just (TypeRef (RefLocal (Name "Int")))) True (DoBlock $ BlockStmt [ExprAsStmt (Identifier (Name "x")),ExprAsStmt (Identifier (Name "y"))])) statementParser

    TestList [ testInlineNoType
             , testInlineNoTypeMutable
             , testInline
             , testInlineValue
             , testDoBlock
             ]

testAssignParserMultiple :: Test
testAssignParserMultiple = do
    let codeInlineNoType = "let x,y = z"
    let testInlineNoType = testParseSuccess codeInlineNoType (AssignMultiple [Name "x",Name "y"] Nothing True (Inline (Identifier (Name "z")))) statementParser

    let codeInlineNoTypeMutable = "let mutable x,y = z"
    let testInlineNoTypeMutable = testParseSuccess codeInlineNoTypeMutable (AssignMultiple [Name "x",Name "y"] Nothing False (Inline (Identifier (Name "z")))) statementParser

    let codeInline = "let x,y: Int = z"
    let testInline = testParseSuccess codeInline (AssignMultiple [Name "x",Name "y"] (Just (TypeRef (RefLocal (Name "Int")))) True (Inline (Identifier (Name "z")))) statementParser

    let codeDoBlockNoType = unlines [ "let x,y = do"
                              , "    i"
                              , "    j"
                              ]
    let testDoBlockNoType = testParseSuccess codeDoBlockNoType (AssignMultiple [Name "x",Name "y"] Nothing True (DoBlock $ BlockStmt [ExprAsStmt (Identifier (Name "i")),ExprAsStmt (Identifier (Name "j"))])) statementParser

    let codeDoBlock = unlines [ "let x,y: Int = do"
                              , "    i"
                              , "    j"
                              ]
    let testDoBlock = testParseSuccess codeDoBlock (AssignMultiple [Name "x",Name "y"] (Just (TypeRef (RefLocal (Name "Int")))) True (DoBlock $ BlockStmt [ExprAsStmt (Identifier (Name "i")),ExprAsStmt (Identifier (Name "j"))])) statementParser

    TestList [ testInlineNoType
             , testInlineNoTypeMutable
             , testInline
             , testDoBlockNoType
             , testDoBlock
             ]
