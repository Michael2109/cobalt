module ParserTests where

import Test.HUnit

-- BaseParserTests
import BaseParserTest

-- ABExprParserTests
import AExprParserTest
import BExprParserTest
import RExprParserTest

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
import ForLoopParserTest
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
import ParenthesesParserTest
import ReassignParserTest
import StringLiteralMultilineParserTest
import StringLiteralParserTest
import ThisMethodCallParserTest
import ThisVarParserTest
import TraitParserTest
import ValueTypeParserTest


parserTestList = TestList [

       -- BaseParser
       testSymbolSingle,
       testSymbolMultiple,
       testSymbolFail,
       testReservedWord,
       testIdentifier,
       testIdentifierFail,
       testFloat,
       testDouble,
       testInteger,
       testLong,

        -- AExprParser
        testAExprParserVar,
        testAExprParserInt,
        testAExprParserNeg,

        -- BExprParser
        testBExprParserTrue,
        testBExprParserFalse,
        testBExprParserFail,

        -- RExprParser
        testRExprParserGreaterVar,
        testRExprParserLessVar,
        testRExprParserGreaterEqualVar,
        testRExprParserLessEqualVar,
        testRExprParserGreaterInt,
        testRExprParserLessInt,
        testRExprParserGreaterEqualInt,
        testRExprParserLessEqualInt,

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
        testClassParserImplementsMultiple,
        testClassParserExtendsImplements,
        testClassParserExtendsImplementsMultiple,
        testClassParserImports,
        testClassParserImportsFail,
        testClassParserModifierBlock,

        testClassVariableParser,

        testForLoopParser,

        testIdentifierParserOneCharacter,
        testIdentifierParserDigitFail,
        testIdentifierParserContainsUnderscore,
        testIdentifierParserContainsDigit,
        testIdentifierParserStartsDigitFail,
        testIdentifierParserCapital,

        testIfStmtParserBooleanTrue,
        testIfStmtParserBooleanFalse,
        testIfStmtParserObjectVar,

        testImportParserSingle,
        testImportParserEmptyFail,
        testImportParserTwo,
        testImportParserMultiple,
        testImportParserStartsDigitFail,
        testImportParserStartsDigitMultipleFail,
        testImportParserCapital,
        testImportParserUnderscore,
        testImportParserMultipleUnderscore,
        testImportParserContainsDigit,

        testObjectMethodCallParserThis,
        testObjectMethodCallParserObject,
        testObjectMethodCallParserSuper,

        testParenthesesParserVar,
        testParenthesesParserNested,
        testParenthesesParserNoOpenFail,
        testParenthesesParserNoCloseFail,

        testStringLiteralSimple,
        testStringLiteralSimpleWhitespace,
        testStringLiteralEscapeTab,
        testStringLiteralEmpty,
        testStringLiteralNewLine,
        testStringLiteralMultipleNewLine,
        testStringLiteralUnescapedSingleQuote,
        testStringLiteralEscapedSingleQuote,
        testStringLiteralEscapedDoubleQuote,
        testStringLiteralDoubleQuoteMultiple,
        testStringLiteralUnfinishedFail
        --testStringLiteralUnfinishedDoubleLineLeadingWhitespaceFail,
        --testStringLiteralUnfinishedDoubleLineFail,

        {--
        testStringLiteralMultilineSimple,
        testStringLiteralMultilineSimpleWhitespace,
        testStringLiteralMultilineEscapeTab,
        testStringLiteralMultilineEmpty,
        testStringLiteralMultilineNewLine,
        testStringLiteralMultilineMultipleNewLine,
        testStringLiteralMultilineUnescapedSingleQuote,
        testStringLiteralMultilineEscapedSingleQuote,
        testStringLiteralMultilineEscapedDoubleQuote,
        testStringLiteralMultilineDoubleQuoteMultiple,
        testStringLiteralMultilineUnfinishedFail,
        --testStringLiteralMultilineUnfinishedDoubleLineLeadingWhitespaceFail,
        --testStringLiteralMultilineUnfinishedDoubleLineFail,
        testStringLiteralMultilineExcludingLeft,
        --}

       ]