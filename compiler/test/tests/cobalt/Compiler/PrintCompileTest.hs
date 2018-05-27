module Compiler.PrintCompileTest where

import Test.HUnit (Assertion, assertBool)

import TestUtil.TestUtil

testCompilePrint :: Assertion
testCompilePrint = do
    results <- executeBytecode "integration/code_generation/Println"

    assertBool "Print statement check failed" (results == unlines [ "11" ])
