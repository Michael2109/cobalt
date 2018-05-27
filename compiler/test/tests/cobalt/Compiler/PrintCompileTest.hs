module Compiler.PrintCompileTest where

import Test.HUnit (Assertion, assertBool)

import TestUtil.TestUtil

testPrintCompile :: Assertion
testPrintCompile = do
    results <- executeBytecode "integration/code_generation/Println"

    assertBool "Class file isn't generated" (results == unlines [ "11" ])
