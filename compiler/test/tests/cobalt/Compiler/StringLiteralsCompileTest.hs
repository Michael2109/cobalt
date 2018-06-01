module Compiler.StringLiteralsCompileTest where

import Test.HUnit (Assertion, assertEqual)

import TestUtil.TestUtil

testCompileStringLiterals :: Assertion
testCompileStringLiterals = do
    results <- executeBytecode "integration/code_generation/StringLiterals"

    let expectedResult = unlines [ "String"
                                 , "String with whitespace"
                                 ]
    assertEqual "String Literals check failed" expectedResult results
