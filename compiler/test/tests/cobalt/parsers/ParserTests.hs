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
import ArrayTypeParserTest
import AssignParserTest
import BooleanParserTest
import ClassParserTest
import ClassVariableParserTest
import ConstructorParserTest
import ForLoopParserTest
import MethodParserTest
import GlobalVariableParserTest
import IdentifierParserTest
import IfElseStatementParserTest
import ImportParserTest
import ModifierBlockParserTest
import NewClassInstanceParserTest
import ObjectMethodCallParserTest
import ObjectParserTest
import ParameterizedTypeParserTest
import ParameterParserTest
import ParenthesesParserTest
import ReassignParserTest
import StringLiteralMultilineParserTest
import StringLiteralParserTest
import ThisMethodCallParserTest
import ThisVarParserTest
import TraitParserTest
import TypeParameterParserTest
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

        testArrayTypeParser,
        testArrayTypeParserEmptyFail,
        testArrayTypeParserNoOpenFail,
        testArrayTypeParserNoCloseFail,
        testArrayTypeParserStartsDigitFail,
        testArrayTypeParserContainsDigit,
        testArrayTypeParserContainsUnderscore,

        testAssignParserValWithType,
        testAssignParserValWithoutType,
        testAssignParserWithoutVal,
        testAssignParserVarWithType,
        testAssignParserVarWithoutType,

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
        testClassParserTypeParameter,

        testClassParserTypeParameterExtends,
        testClassParserTypeParameterExtendsImplements,
        testClassParserTypeParameterImplements,
        testClassParserTypeParameterImplementsMultiple,
        testClassParserExtends,
        testClassParserImplements,
        testClassParserImplementsMultiple,
        testClassParserExtendsImplements,
        testClassParserExtendsImplementsMultiple,
        testClassParserImports,
        testClassParserImportsFail,
        testClassParserModifierBlock,

        testClassVariableParser,

        testClassVariableParser,
        testClassVariableParserUnderscores,
        testClassVariableParserStartCapitals,
        testClassVariableParserMissingVar,
        testClassVariableParserMissingClassName,

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
        testIfStmtParserAnd,
        testIfStmtParserOr,
        testIfStmtParserAndOr,

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

        testMethodParser,
        testMethodParserEmptyParams,
        testMethodParserMissingParens,
        testMethodParserMissingName,
        testMethodParserMissingReturnType,

        testModifierBlockParserPrivate,
        testModifierBlockParserProtected,
        testModifierBlockParserPublic,
        testModifierBlockParserPrivateEmpty,
        testModifierBlockParserProtectedEmpty,
        testModifierBlockParserPublicEmpty,

        testObjectParserTypeParameterExtends,
        testObjectParserTypeParameterExtendsImplements,
        testObjectParserTypeParameterImplements,
        testObjectParserTypeParameterImplementsMultiple,
        testObjectParserExtends,
        testObjectParserImplements,
        testObjectParserImplementsMultiple,
        testObjectParserExtendsImplements,
        testObjectParserExtendsImplementsMultiple,
        testObjectParserImports,
        testObjectParserImportsFail,
        testObjectParserModifierBlock,

        testObjectMethodCallParserThis,
        testObjectMethodCallParserObject,
        testObjectMethodCallParserSuper,

        testParameterParser,
        testParameterParserMissingVar,
        testParameterParserMissingType,
        testParameterParserMissingColon,

        testParameterizedTypeParser,
        testParameterizedTypeParserLeftMissing,
        testParameterizedTypeParserRightMissing,
        testParameterizedTypeParserClassMissing,

        testParenthesesParserVar,
        testParenthesesParserNested,
        testParenthesesParserNoOpenFail,
        testParenthesesParserNoCloseFail,

        testReassignParserObject,
        testReassignParserArithmetic,
        testReassignParserClassVar,

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
        testStringLiteralUnfinishedFail,
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
        testStringLiteralMultilineUnfinishedDoubleLineLeadingWhitespaceFail,
        testStringLiteralMultilineUnfinishedDoubleLineFail,
        testStringLiteralMultilineExcludingLeft,
        --}

        testThisVarParserStartsDigitFail,
        testThisVarParserContainsCapital,
        testThisVarParserContainsDigit,
        testThisVarParserContainsUnderscore,
        testThisVarParserNotThisFail,

        testTraitParserTypeParameterExtends,
        testTraitParserTypeParameterExtendsImplements,
        testTraitParserTypeParameterImplements,
        testTraitParserTypeParameterImplementsMultiple,
        testTraitParserExtends,
        testTraitParserImplements,
        testTraitParserImplementsMultiple,
        testTraitParserExtendsImplements,
        testTraitParserExtendsImplementsMultiple,
        testTraitParserImports,
        testTraitParserImportsFail,
        testTraitParserModifierBlock,

        testTypeParameterParser,
        testTypeParameterParserMissingLeft,
        testTypeParameterParserMissingRight,
        testTypeParameterParserMissingBoth

       ]