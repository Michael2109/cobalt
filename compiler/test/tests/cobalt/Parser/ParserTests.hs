module Parser.ParserTests where

import Test.HUnit

import Parser.BaseParserTest
import Parser.AExprParserTest
import Parser.BExprParserTest
import Parser.RExprParserTest
import Parser.AnnotationParserTest
import Parser.ArgumentParserTest
import Parser.ArgumentTypeParserTest
import Parser.ArithmeticParserTest
import Parser.AssignParserTest
import Parser.BooleanParserTest
import Parser.ClassParserTest
import Parser.ClassVariableParserTest
import Parser.ForLoopParserTest
import Parser.IdentifierParserTest
import Parser.IfElseStatementParserTest
import Parser.ImportParserTest
import Parser.MethodCallParserTest
import Parser.MethodParserTest
import Parser.Data.ModelTypeParserTest
import Parser.Data.ModifierParserTest
import Parser.ModifierBlockParserTest
import Parser.NewClassInstanceParserTest
import Parser.ObjectMethodCallParserTest
import Parser.ObjectParserTest
import Parser.PackageParserTest
import Parser.ParameterizedTypeParserTest
import Parser.ParameterParserTest
import Parser.ParenthesesParserTest
import Parser.ReassignParserTest
import Parser.StringLiteralParserTest
import Parser.ThisVarParserTest
import Parser.TraitParserTest
import Parser.TypeParameterParserTest
import Parser.ValueTypeParserTest

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

    , testAssignParserTwoVars
    , testAssignParserThreeVars
    , testAssignParserFourVars

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
    , testClassParserConstructorBody
    , testClassParserMethods

    , testClassParserPublic
    , testClassParserProtected
    , testClassParserPrivate
    , testClassParserPublicAbstract
    , testClassParserPrivate
    , testClassParserPrivateAbstract
    , testClassParserAbstract
    , testClassParserPublicFinal
    , testClassParserProtectedFinal
    , testClassParserPrivateFinal
    , testClassParserFinal
    , testClassParserReordered1
    , testClassParserReordered2
    , testClassParserReordered3

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
    , testIfStmtParserElif
    , testIfStmtParserElse
    , testIfStmtParserBlockIf
    , testIfStmtParserBlockElif
    , testIfStmtParserBlockElifElse
    , testIfStmtParserBlockElse

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
    , testMethodParserModifierPublic
    , testMethodParserModifierProtected
    , testMethodParserModifierPrivate
    , testMethodParserModifierPublicAbstract
    , testMethodParserModifierProtectedAbstract
    , testMethodParserModifierPrivateAbstract
    , testMethodParserModifierAbstract
    , testMethodParserModifierPublicFinal
    , testMethodParserModifierProtectedFinal
    , testMethodParserModifierPrivateFinal
    , testMethodParserModifierFinal
    , testMethodParserModifierReordered1
    , testMethodParserModifierReordered2
    , testMethodParserModifierReordered3

    , testAccessModifierParserPublic
    , testAccessModifierParserProtected
    , testAccessModifierParserPrivate
    , testAbstractModifierParser
    , testFinalModifierParser

    , testModelTypeParserClass
    , testModelTypeParserObject
    , testModelTypeParserTrait

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
    , testObjectParserPublic
    , testObjectParserProtected
    , testObjectParserPrivate
    , testObjectParserPublicAbstract
    , testObjectParserProtectedAbstract
    , testObjectParserPrivateAbstract
    , testObjectParserAbstract
    , testObjectParserPublicFinal
    , testObjectParserProtectedFinal
    , testObjectParserPrivateFinal
    , testObjectParserFinal
    , testObjectParserReordered1
    , testObjectParserReordered2
    , testObjectParserReordered3

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
    , testReassignParserArithmeticTwoVars
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
    , testTraitParserPublic
    , testTraitParserProtected
    , testTraitParserPrivate
    , testTraitParserPublicAbstract
    , testTraitParserProtectedAbstract
    , testTraitParserPrivateAbstract
    , testTraitParserAbstract
    , testTraitParserPublicFinal
    , testTraitParserProtectedFinal
    , testTraitParserPrivateFinal
    , testTraitParserFinal
    , testTraitParserReordered1
    , testTraitParserReordered2
    , testTraitParserReordered3

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
