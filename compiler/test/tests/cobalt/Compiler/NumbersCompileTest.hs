module Compiler.NumbersCompileTest where

import Test.HUnit (Assertion, assertEqual)

import TestUtil.TestUtil

testCompileNumbers :: Assertion
testCompileNumbers = do
    results <- executeBytecode "integration/code_generation/Numbers"

    let expectedResult = unlines [ "2147483646"
                                 , "9223372036854775806"
                                 , "5.0"
                                 , "10.4"
                                 ]
    assertEqual "Numbers check failed" expectedResult results
