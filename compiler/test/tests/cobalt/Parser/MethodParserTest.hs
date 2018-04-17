module Parser.MethodParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import AST.AST
import Parser.ExprParser

testMethodParser :: Test
testMethodParser = do
    let code = "exampleMethod (a: Int, b: Int): Int"
    TestCase $ assertEqual code
        ( Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [Field {fieldName = Name "a", fieldType = TypeRef (RefLocal (Name "Int")), fieldInit = Nothing},Field {fieldName = Name "b", fieldType = TypeRef (RefLocal (Name "Int")), fieldInit = Nothing}], methodReturnType = TypeRef (RefLocal (Name "Int")),methodBody = Block []})
        (case (parse methodParser "" code) of
             Left  e -> error $ show e
             Right x -> x)

testMethodParserEmptyParams :: Test
testMethodParserEmptyParams = do
    let code = "exampleMethod (): Int"
    TestCase $ assertEqual code
        (Method {methodName = Name "exampleMethod", methodAnns = [], methodParams = [], methodReturnType = TypeRef (RefLocal (Name "Int")), methodBody = Block []})
        (case (parse methodParser "" code) of
             Left  e -> error $ show e
             Right x -> x)

{-
testMethodParserEmptyParams :: Test
testMethodParserEmptyParams = do
    let code = unlines [ "exampleMethod (): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [] [] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserMissingParens :: Test
testMethodParserMissingParens = do
    let code = unlines [ "exampleMethod : Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [] [] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserMissingName :: Test
testMethodParserMissingName = do
    let code = unlines [ "(a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        Error
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserMissingReturnType :: Test
testMethodParserMissingReturnType = do
    let code = unlines [ "exampleMethod(a: Int, b: Int)"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        Error
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

-- Modifier tests
testMethodParserModifierPublic :: Test
testMethodParserModifierPublic = do
    let code = unlines [ "public exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Public] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserModifierProtected :: Test
testMethodParserModifierProtected = do
    let code = unlines [ "protected exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Protected] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserModifierPrivate :: Test
testMethodParserModifierPrivate = do
    let code = unlines [ "private exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Private] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserModifierPublicAbstract :: Test
testMethodParserModifierPublicAbstract = do
    let code = unlines [ "public abstract exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Public, Abstract] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserModifierProtectedAbstract :: Test
testMethodParserModifierProtectedAbstract = do
    let code = unlines [ "protected abstract exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Protected, Abstract] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserModifierPrivateAbstract :: Test
testMethodParserModifierPrivateAbstract = do
    let code = unlines [ "private abstract exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Private, Abstract] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserModifierAbstract :: Test
testMethodParserModifierAbstract = do
    let code = unlines [ "abstract exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Abstract] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserModifierPublicFinal :: Test
testMethodParserModifierPublicFinal = do
    let code = unlines [ "public final exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Public, Final] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserModifierProtectedFinal :: Test
testMethodParserModifierProtectedFinal = do
    let code = unlines [ "protected final exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Protected, Final] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserModifierPrivateFinal:: Test
testMethodParserModifierPrivateFinal = do
    let code = unlines [ "private final exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Private, Final] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserModifierFinal :: Test
testMethodParserModifierFinal = do
    let code = unlines [ "final exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Final] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserModifierReordered1 :: Test
testMethodParserModifierReordered1 = do
    let code = unlines [ "abstract public exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Abstract, Public] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserModifierReordered2 :: Test
testMethodParserModifierReordered2 = do
    let code = unlines [ "final abstract public exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Final, Abstract, Public] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)

testMethodParserModifierReordered3 :: Test
testMethodParserModifierReordered3 = do
    let code = unlines [ "abstract private final exampleMethod (a: Int, b: Int): Int"
                       , "  println(\"Hello world\")"
                       ]
    TestCase $ assertEqual code
        (Method (Identifier "exampleMethod") Nothing [Abstract, Private, Final] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [Print (StringLiteral "Hello world")])
        (case (parse (methodParser "ModuleName" False) "" code) of
             Left  _ -> Error
             Right x -> x)
-}
