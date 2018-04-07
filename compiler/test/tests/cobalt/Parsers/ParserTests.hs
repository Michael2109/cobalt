module Parsers.ParserTests where

import Test.HUnit

import Parsers.BaseParserTest
import Parsers.AExprParserTest
import Parsers.BExprParserTest
import Parsers.RExprParserTest
import Parsers.AnnotationParserTest
import Parsers.ArgumentParserTest
import Parsers.ArgumentTypeParserTest
import Parsers.ArithmeticParserTest
import Parsers.AssignParserTest
import Parsers.BooleanParserTest
import Parsers.ClassParserTest
import Parsers.ClassVariableParserTest
import Parsers.ForLoopParserTest
import Parsers.IdentifierParserTest
import Parsers.IfElseStatementParserTest
import Parsers.ImportParserTest
import Parsers.MethodCallParserTest
import Parsers.MethodParserTest
import Parsers.ModifierBlockParserTest
import Parsers.NewClassInstanceParserTest
import Parsers.ObjectMethodCallParserTest
import Parsers.ObjectParserTest
import Parsers.PackageParserTest
import Parsers.ParameterizedTypeParserTest
import Parsers.ParameterParserTest
import Parsers.ParenthesesParserTest
import Parsers.ReassignParserTest
import Parsers.StringLiteralParserTest
import Parsers.ThisVarParserTest
import Parsers.TraitParserTest
import Parsers.TypeParameterParserTest
import Parsers.ValueTypeParserTest

parserTestList :: Test
parserTestList = TestList
    -- BaseParser
    [ testSymbolSingle
    , testSymbolMultiple
    , testSymbolFail
    , testReservedWord
    , testIdentifier
    , testIdentifierFail
    , testFloat
    , testDouble
    , testInteger
    , testLong

    -- AExprParser
    , testAExprParserVar
    , testAExprParserInt
    , testAExprParserNeg

    -- BExprParser
    , testBExprParserTrue
    , testBExprParserFalse
    , testBExprParserFail

    -- RExprParser
    , testRExprParserGreaterVar
    , testRExprParserLessVar
    , testRExprParserGreaterEqualVar
    , testRExprParserLessEqualVar
    , testRExprParserGreaterInt
    , testRExprParserLessInt
    , testRExprParserGreaterEqualInt
    , testRExprParserLessEqualInt

    -- ExprParser
    , testAnnotationParserUpper
    , testAnnotationParserLower

    , testArgumentParserIdentifier
    , testArgumentParserBoolTrue
    , testArgumentParserBoolFalse

    , testArgumentTypeParser
    , testArgumentTypeParserReservedWord

    , testArithmeticParserIdentifier
    , testArithmeticParserClassVariable
    , testArithmeticParserNewInstance
    , testArithmeticParserMethodCall
    , testArithmeticParserAdd
    , testArithmeticParserSubtract
    , testArithmeticParserMultiply
    , testArithmeticParserDivide

    , testAssignParserValWithType
    , testAssignParserValWithoutType
    , testAssignParserWithoutVal
    , testAssignParserVarWithType
    , testAssignParserVarWithoutType
    , testAssignParserValWithParameterizedType
    , testAssignParserVarWithParameterizedType

    , testBooleanParserTrue
    , testBooleanParserFalse
    , testBooleanParserIdentifier
    , testBooleanParserLessThanVar
    , testBooleanParserLessThanInt
    , testBooleanParserGreaterThanVar
    , testBooleanParserGreaterThanInt
    , testBooleanParserLessThanEqualVar
    , testBooleanParserLessThanEqualInt
    , testBooleanParserGreaterThanEqualVar
    , testBooleanParserGreaterThanEqualInt

    , testClassParser
    , testClassParserTypeParameter

    , testClassParserTypeParameterExtends
    , testClassParserTypeParameterExtendsImplements
    , testClassParserTypeParameterImplements
    , testClassParserTypeParameterImplementsMultiple
    , testClassParserExtends
    , testClassParserImplements
    , testClassParserImplementsMultiple
    , testClassParserExtendsImplements
    , testClassParserExtendsImplementsMultiple
    , testClassParserImports
    , testClassParserImportsFail
    , testClassParserModifierBlock

    , testClassVariableParser

    , testClassVariableParser
    , testClassVariableParserUnderscores
    , testClassVariableParserStartCapitals
    , testClassVariableParserMissingVar
    , testClassVariableParserMissingClassName

    , testForLoopParser

    , testIdentifierParserOneCharacter
    , testIdentifierParserDigitFail
    , testIdentifierParserContainsUnderscore
    , testIdentifierParserContainsDigit
    , testIdentifierParserStartsDigitFail
    , testIdentifierParserCapital

    , testIfStmtParserBooleanTrue
    , testIfStmtParserBooleanFalse
    , testIfStmtParserObjectVar
    , testIfStmtParserAnd
    , testIfStmtParserOr
    , testIfStmtParserAndOr

    , testImportParserSingle
    , testImportParserEmptyFail
    , testImportParserTwo
    , testImportParserMultiple
    , testImportParserStartsDigitFail
    , testImportParserStartsDigitMultipleFail
    , testImportParserCapital
    , testImportParserUnderscore
    , testImportParserMultipleUnderscore
    , testImportParserContainsDigit

    , testMethodCallParser
    , testMethodParser
    , testMethodParserEmptyParams
    , testMethodParserMissingParens
    , testMethodParserMissingName
    , testMethodParserMissingReturnType

    , testModifierBlockParserPrivate
    , testModifierBlockParserProtected
    , testModifierBlockParserPublic
    , testModifierBlockParserPrivateEmpty
    , testModifierBlockParserProtectedEmpty
    , testModifierBlockParserPublicEmpty

    , testNewClassInstanceParserNoArgs
    , testNewClassInstanceParserNoArgsUnderscore
    , testNewClassInstanceParserNoArgsLowerCase
    , testNewClassInstanceParserNewUpperCase
    , testNewClassInstanceParserNoArgsNoParens
    , testNewClassInstanceParserSingleArg
    , testNewClassInstanceParserMultiArgs
    , testNewClassInstanceParserMissingNew
    , testNewClassInstanceParserMissingLeftParen
    , testNewClassInstanceParserMissingRightParen

    , testObjectParserTypeParameterExtends
    , testObjectParserTypeParameterExtendsImplements
    , testObjectParserTypeParameterImplements
    , testObjectParserTypeParameterImplementsMultiple
    , testObjectParserExtends
    , testObjectParserImplements
    , testObjectParserImplementsMultiple
    , testObjectParserExtendsImplements
    , testObjectParserExtendsImplementsMultiple
    , testObjectParserImports
    , testObjectParserImportsFail
    , testObjectParserModifierBlock

    , testObjectMethodCallParserThis
    , testObjectMethodCallParserObject
    , testObjectMethodCallParserSuper

    , testPackageParser

    , testParameterParser
    , testParameterParserMissingVar
    , testParameterParserMissingType
    , testParameterParserMissingColon

    , testParameterizedTypeParser
    , testParameterizedTypeParserLeftMissing
    , testParameterizedTypeParserRightMissing
    , testParameterizedTypeParserClassMissing

    , testParenthesesParserVar
    , testParenthesesParserNested
    , testParenthesesParserNoOpenFail
    , testParenthesesParserNoCloseFail

    , testReassignParserObject
    , testReassignParserArithmetic
    , testReassignParserClassVar

    , testStringLiteralSimple
    , testStringLiteralSimpleWhitespace
    , testStringLiteralEscapeTab
    , testStringLiteralEmpty
    , testStringLiteralNewLine
    , testStringLiteralMultipleNewLine
    , testStringLiteralUnescapedSingleQuote
    , testStringLiteralEscapedSingleQuote
    , testStringLiteralEscapedDoubleQuote
    , testStringLiteralDoubleQuoteMultiple
    , testStringLiteralUnfinishedFail
    --testStringLiteralUnfinishedDoubleLineLeadingWhitespaceFail,
    --testStringLiteralUnfinishedDoubleLineFail,

    {--
    , testStringLiteralMultilineSimple,
    , testStringLiteralMultilineSimpleWhitespace,
    , testStringLiteralMultilineEscapeTab,
    , testStringLiteralMultilineEmpty,
    , testStringLiteralMultilineNewLine,
    , testStringLiteralMultilineMultipleNewLine,
    , testStringLiteralMultilineUnescapedSingleQuote,
    , testStringLiteralMultilineEscapedSingleQuote,
    , testStringLiteralMultilineEscapedDoubleQuote,
    , testStringLiteralMultilineDoubleQuoteMultiple,
    , testStringLiteralMultilineUnfinishedFail,
    , testStringLiteralMultilineUnfinishedDoubleLineLeadingWhitespaceFail,
    , testStringLiteralMultilineUnfinishedDoubleLineFail,
    , testStringLiteralMultilineExcludingLeft,
    --}

    , testThisVarParserStartsDigitFail
    , testThisVarParserContainsCapital
    , testThisVarParserContainsDigit
    , testThisVarParserContainsUnderscore
    , testThisVarParserNotThisFail

    , testTraitParserTypeParameterExtends
    , testTraitParserTypeParameterExtendsImplements
    , testTraitParserTypeParameterImplements
    , testTraitParserTypeParameterImplementsMultiple
    , testTraitParserExtends
    , testTraitParserImplements
    , testTraitParserImplementsMultiple
    , testTraitParserExtendsImplements
    , testTraitParserExtendsImplementsMultiple
    , testTraitParserImports
    , testTraitParserImportsFail
    , testTraitParserModifierBlock

    , testTypeParameterParser
    , testTypeParameterParserMissingLeft
    , testTypeParameterParserMissingRight
    , testTypeParameterParserMissingBoth

    , testValueTypeParserOneCharacter
    , testValueTypeParserDigitFail
    , testValueTypeParserContainsUnderscore
    , testValueTypeParserContainsDigit
    , testValueTypeParserStartsDigitFail
    , testValueTypeParserCapital
    ]
