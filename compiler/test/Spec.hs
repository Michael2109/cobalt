import Test.HUnit

-- BaseParserTests
import BaseParserTest

-- ABExprParserTests
import AExprParserTest
import BExprParserTest

-- ExprParserTests
import AnnotationParserTest
import ArgumentParserTest
import ArgumentTypeParserTest
import ArithmeticParserTest
import ArrayAppendParserTest
import ArrayElementSelectParserTest
import ArrayValuesParserTest
import AssignParserTest
import BooleanParserTest
import ClassParserTest
import ClassVariableParserTest
import ConstructorParserTest
import FunctionParserTest
import GlobalVariableParserTest
import IdentifierParserTest
import IfElseStatementParserTest
import ImportParserTest
import ModifierBlockParserTest
import NewClassInstanceParserTest
import ObjectMethodCallParserTest
import ObjectParserTest
import ParamParserTest
import ReassignParserTest
import StringLiteralParserTest
import ThisMethodCallParserTest
import ThisVarParserTest
import TraitParserTest
import ValueTypeParserTest


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

   testArgumentParserIdentifier,
   testArgumentParserBoolTrue,
   testArgumentParserBoolFalse,

   testArgumentTypeParser,
   testArgumentTypeParserReservedWord,

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
   testBooleanParserGreaterThanEqualInt,

   testClassParser,
   testClassParserExtends,
   testClassParserImplements,
   testClassParserExtendsImplements,
   testClassParserImports,
   testClassParserImportsFail,
   testClassParserModifierBlock,

   testClassVariableParser,

   testIfStmtParserBooleanTrue,
   testIfStmtParserBooleanFalse
  ]


