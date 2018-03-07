import Test.HUnit

-- BaseParserTests
import BaseParserTest

-- ABExprParserTests
import AExprParserTest
import BExprParserTest

-- ExprParserTests
import AnnotationParserTest
import BooleanParserTest


main :: IO Counts
main = runTestTT $ TestList [

  -- BaseParser
  testSymbolSingle,
  testSymbolMultiple,
  testSymbolFail,
  testReservedWord,
  testIdentifier,
  testIdentifierFail,

   -- AExprParser
   testAExprParserVar,
   testAExprParserInt,
   testAExprParserNeg,

   -- BExprParser
   testBExprParserTrue,
   testBExprParserFalse,
   testBExprParserFail,

   -- ExprParser
   testAnnotationParserUpper,
   testAnnotationParserLower,

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


