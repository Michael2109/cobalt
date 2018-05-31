module Compiler.ArithmeticCompileTest where

import Test.HUnit (Assertion, assertEqual)

import TestUtil.TestUtil

testCompileArithmetic :: Assertion
testCompileArithmetic = do
    results <- executeBytecode "integration/code_generation/Arithmetic"

    assertEqual "Arithmetic check failed" (unlines [ "2" ]) results
