import Test.HUnit

import BaseParserTest
import ABExprParserTest

main :: IO Counts
main = runTestTT $ TestList [
  testBooleanParserTrue,
  testBooleanParserFalse,
  testBooleanParserFail
  ]