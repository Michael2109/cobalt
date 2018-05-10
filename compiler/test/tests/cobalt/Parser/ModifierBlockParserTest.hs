module Parser.ModifierBlockParserTest where

import Test.HUnit


import Parser.Parser
{-

testModifierBlockParserPrivate :: Test
testModifierBlockParserPrivate = do
    let code = unlines [ "private"
                       , "  let x: int = 5"
                       , "  let y: int = 10"
                       ]
    TestCase $ assertEqual code
        (ModifierBlock [Assign True (Just (Type (Identifier "int"))) (Identifier "x") (IntConst 5),Assign True (Just (Type (Identifier "int"))) (Identifier "y") (IntConst 10)])
        (case (parse (modifierBlockParser True) "" code) of
              Left  _ -> Error
              Right x -> x)

testModifierBlockParserProtected :: Test
testModifierBlockParserProtected = do
    let code = unlines [ "protected"
                       , "  let x: int = 5"
                       , "  let y: int = 10"
                       ]
    TestCase $ assertEqual code
        (ModifierBlock [Assign True (Just (Type (Identifier "int"))) (Identifier "x") (IntConst 5),Assign True (Just (Type (Identifier "int"))) (Identifier "y") (IntConst 10)])
        (case (parse (modifierBlockParser True) "" code) of
             Left  _ -> Error
             Right x -> x)

testModifierBlockParserPublic :: Test
testModifierBlockParserPublic = do
    let code = unlines [ "public"
                       , "  let x: int = 5"
                       , "  let y: int = 10"
                       ]
    TestCase $ assertEqual code
        (ModifierBlock [Assign True (Just (Type (Identifier "int"))) (Identifier "x") (IntConst 5),Assign True (Just (Type (Identifier "int"))) (Identifier "y") (IntConst 10)])
        (case (parse (modifierBlockParser True) "" code) of
             Left  _ -> Error
             Right x -> x)

testModifierBlockParserPrivateEmpty :: Test
testModifierBlockParserPrivateEmpty = do
    let code = unlines [ "public" ]
    TestCase $ assertEqual code
        (ModifierBlock [])
        (case (parse (modifierBlockParser True) "" code) of
             Left  _ -> Error
             Right x -> x)

testModifierBlockParserProtectedEmpty :: Test
testModifierBlockParserProtectedEmpty = do
    let code = unlines [ "public" ]
    TestCase $ assertEqual code
        (ModifierBlock [])
        (case (parse (modifierBlockParser True) "" code) of
             Left  _ -> Error
             Right x -> x)

testModifierBlockParserPublicEmpty :: Test
testModifierBlockParserPublicEmpty = do
    let code = unlines [ "public" ]
    TestCase $ assertEqual code
        (ModifierBlock [])
        (case (parse (modifierBlockParser True) "" code) of
             Left  _ -> Error
             Right x -> x)
-}
