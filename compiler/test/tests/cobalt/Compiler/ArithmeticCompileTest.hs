module Compiler.ArithmeticCompileTest where

import Test.HUnit (Assertion, assertEqual)

import TestUtil.TestUtil

testCompileArithmetic :: Assertion
testCompileArithmetic = do
    results <- executeBytecode "integration/code_generation/Arithmetic"

    let expectedResult = "8\n8\n8.7\n8.7\n1\n-6\n-6.7\n-6.7\n40\n10\n13.0\n13.0\n1\n0\n0.07692308\n0.07692307692307693\n2\n-2\n2.1999998\n1.492156862745098\n-2\n-2\n-1.7142857\n-1.7142857142857144\n"
    assertEqual "Arithmetic check failed" expectedResult results
