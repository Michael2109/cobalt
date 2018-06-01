module Compiler.ArithmeticCompileTest where

import Test.HUnit (Assertion, assertEqual)

import TestUtil.TestUtil

testCompileArithmetic :: Assertion
testCompileArithmetic = do
    results <- executeBytecode "integration/code_generation/Arithmetic"

    let expectedResult = unlines [ "8"
                                 , "1"
                                 , "40"
                                 , "1"
                                 , "2"
                                 , "-2"
                                 ]
    assertEqual "Arithmetic check failed" expectedResult results
