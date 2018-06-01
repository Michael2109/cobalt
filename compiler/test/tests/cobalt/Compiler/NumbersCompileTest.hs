module Compiler.NumbersCompileTest where

import Test.HUnit (Assertion, assertEqual)

import TestUtil.TestUtil

testCompileNumbers :: Assertion
testCompileNumbers = do
    results <- executeBytecode "integration/code_generation/Numbers"

    assertEqual "Numbers check failed" (unlines [ "11" ]) results
