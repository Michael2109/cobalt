import Test.HUnit

-- ABExprParserTests
import AExprParserTest

-- ExprParserTests
import BooleanParserTest

import BaseParserTest
import ABExprParserTest
import ExprParserTest

main :: IO Counts
main = runTestTT $ TestList [
   -- AExprParser tests
   testAExprParserVar,
   testAExprParserInt,
   testAExprParserNeg,

   -- ExprParser tests
   testBooleanParserTrue,
   testBooleanParserFalse,
   testBooleanParserIdentifier,
   testBooleanParserLessThanVar,
   testBooleanParserLessThanInt,
   testBooleanParserGreaterThanVar,
   testBooleanParserGreaterThanInt,
   testBooleanParserLessThanEqualVar,
   testBooleanParserLessThanEqualInt,
   testBooleanParserGreaterThanEqualVar,
   testBooleanParserGreaterThanEqualInt
  ]


