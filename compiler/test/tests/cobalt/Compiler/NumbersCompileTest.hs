module Compiler.NumbersCompileTest where

import Test.HUnit (Assertion, assertBool)

import TestUtil.TestUtil

testCompileNumbers :: Assertion
testCompileNumbers = do
    results <- executeBytecode "integration/code_generation/Numbers"

    assertBool "Print statement check failed" (results == unlines [ "11" ])
