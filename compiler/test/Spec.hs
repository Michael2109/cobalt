import Test.HUnit

import BaseParserTest
import ABExprParserTest
import ExprParserTest

main :: IO Counts
main = runTestTT $ booleanParserTests



booleanParserTests = TestList [
  testBooleanParserTrue,
  testBooleanParserFalse,
  testBooleanParserIdentifier,
  testBooleanParserLessThan,
  testBooleanParserGreaterThan,
  testBooleanParserLessThanEqual,
  testBooleanParserGreaterThanEqual
  ]