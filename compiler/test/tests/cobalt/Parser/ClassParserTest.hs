module Parser.ClassParserTest where

import Test.HUnit
import Text.Megaparsec

import AST.Block
import AST.Data.Modifier
import Parser.ExprParser

testClassParser :: Test
testClassParser = do
    let code = unlines [ "class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserTypeParameter :: Test
testClassParserTypeParameter = do
    let code = unlines [ "class Test[String]" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserTypeParameterExtends :: Test
testClassParserTypeParameterExtends = do
    let code = unlines [ "class Test[String] extends ParentName" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] (Just "ParentName") [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserTypeParameterExtendsImplements :: Test
testClassParserTypeParameterExtendsImplements = do
    let code = unlines [ "class Test[String] extends ParentName implements TraitName" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] (Just "ParentName") ["TraitName"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserTypeParameterImplements :: Test
testClassParserTypeParameterImplements = do
    let code = unlines [ "class Test[String] implements TraitName" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] Nothing ["TraitName"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserTypeParameterImplementsMultiple :: Test
testClassParserTypeParameterImplementsMultiple = do
    let code = unlines [ "class Test[String] implements TraitName1, TraitName2" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" (Just (TypeParameter (Identifier "String"))) [] [] Nothing ["TraitName1","TraitName2"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserExtends :: Test
testClassParserExtends = do
    let code = unlines [ "class Test extends ParentClass" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserImplements :: Test
testClassParserImplements = do
    let code = unlines [ "class Test implements Interface" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] Nothing ["Interface"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserImplementsMultiple :: Test
testClassParserImplementsMultiple = do
    let code = unlines [ "class Test implements Interface1, Interface2" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] Nothing ["Interface1","Interface2"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserExtendsImplements :: Test
testClassParserExtendsImplements = do
    let code = unlines [ "class Test extends ParentClass implements Interface" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") ["Interface"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserExtendsImplementsMultiple :: Test
testClassParserExtendsImplementsMultiple = do
    let code = unlines [ "class Test extends ParentClass implements Interface1, Interface2, Interface3" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") ["Interface1","Interface2","Interface3"] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserImports :: Test
testClassParserImports = do
    let code = unlines [ "import dir.sub_dir.ClassName"
                       , "class Test extends ParentClass implements Interface"
                       ]
    let imports = [Import ["dir", "sub_dir", "ClassName"]]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") ["Interface"] imports [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserImportsFail :: Test
testClassParserImportsFail = do
    let code = unlines [ "import -dir.sub_dir.ClassName"
                       , "class Test extends ParentClass implements Interface"
                       ]
    TestCase $ assertEqual code
        Error
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserModifierBlock :: Test
testClassParserModifierBlock = do
    let code = unlines [ "import dir.sub_dir.ClassName"
                       , "class Test extends ParentClass implements Interface"
                       , "public"
                       , "  val x: int = 5"
                       ]
    let imports = [Import ["dir", "sub_dir", "ClassName"]]
    let modifierBlocks = [ModifierBlock [GlobalVar "public" True False (Type (Identifier "int")) (Identifier "x") [IntConst 5]]]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") ["Interface"] imports modifierBlocks [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserConstructorBody :: Test
testClassParserConstructorBody = do
    let code = unlines [ "import dir.sub_dir.ClassName"
                       , "class Test extends ParentClass implements Interface"
                       , "public"
                       , "  val x: int = 5"
                       , "initAlien(10, 20)"
                       ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") ["Interface"] [Import ["dir","sub_dir","ClassName"]] [ModifierBlock [GlobalVar "public" True False (Type (Identifier "int")) (Identifier "x") [IntConst 5]]] [MethodCall "initAlien" [IntConst 10, IntConst 20]] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserMethods :: Test
testClassParserMethods = do
    let code = unlines [ "import dir.sub_dir.ClassName"
                       , "class Test extends ParentClass implements Interface"
                       , "public"
                       , "    val x: int = 5"
                       , "initAlien(10, 20)"
                       , "exampleMethod (a: Int, b: Int): Int"
                       , "initAlien (x: int, y: int): void"
                       , "    this.x = x"
                       , "    this.y = y"
                       , "act (direction: int): void"
                       ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [] [] (Just "ParentClass") ["Interface"] [Import ["dir","sub_dir","ClassName"]] [ModifierBlock [GlobalVar "public" True False (Type (Identifier "int")) (Identifier "x") [IntConst 5]]] [MethodCall "initAlien" [IntConst 10, IntConst 20]] [Method (Identifier "exampleMethod") Nothing [] [Parameter (Identifier "Int") (Identifier "a"),Parameter (Identifier "Int") (Identifier "b")] (Identifier "Int") False [],Method (Identifier "initAlien") Nothing [] [Parameter (Identifier "int") (Identifier "x"),Parameter (Identifier "int") (Identifier "y")] (Identifier "void") False [Reassign (ThisVar (Identifier "x")) (Identifier "x"),Reassign (ThisVar (Identifier "y")) (Identifier "y")],Method (Identifier "act") Nothing [] [Parameter (Identifier "int") (Identifier "direction")] (Identifier "void") False []])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

-- Modifiers
testClassParserPublic :: Test
testClassParserPublic = do
    let code = unlines [ "public class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Public] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserProtected :: Test
testClassParserProtected = do
    let code = unlines [ "protected class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Protected] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserPrivate :: Test
testClassParserPrivate = do
    let code = unlines [ "private class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Private] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserPublicAbstract :: Test
testClassParserPublicAbstract = do
    let code = unlines [ "public abstract class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Public, Abstract] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserProtectedAbstract :: Test
testClassParserProtectedAbstract = do
    let code = unlines [ "protected abstract class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Protected, Abstract] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserPrivateAbstract :: Test
testClassParserPrivateAbstract = do
    let code = unlines [ "private abstract class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Private, Abstract] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserAbstract :: Test
testClassParserAbstract = do
    let code = unlines [ "abstract class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Abstract] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserPublicFinal :: Test
testClassParserPublicFinal = do
    let code = unlines [ "public final class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Public, Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserProtectedFinal :: Test
testClassParserProtectedFinal = do
    let code = unlines [ "protected final class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Protected, Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserPrivateFinal :: Test
testClassParserPrivateFinal = do
    let code = unlines [ "private final class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Private, Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserFinal :: Test
testClassParserFinal = do
    let code = unlines [ "final class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserReordered1 :: Test
testClassParserReordered1 = do
    let code = unlines [ "abstract public class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Abstract, Public] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserReordered2 :: Test
testClassParserReordered2 = do
    let code = unlines [ "final abstract public class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Final, Abstract, Public] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)

testClassParserReordered3 :: Test
testClassParserReordered3 = do
    let code = unlines [ "abstract private final class Test" ]
    TestCase $ assertEqual code
        (Class Nothing "Test" Nothing [Abstract, Private, Final] [] Nothing [] [] [] [] [])
        (case (parse (modelParser) "" code) of
             Left  _ -> Error
             Right x -> x)