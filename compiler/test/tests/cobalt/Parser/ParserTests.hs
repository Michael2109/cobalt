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
import Parser.ClassVariableParserTest
import Parser.ForLoopParserTest
import Parser.IdentifierParserTest
import Parser.IfElseStatementParserTest
import Parser.ImportParserTest
import Parser.MethodCallParserTest
import Parser.MethodParserTest
import Parser.ModelTypeParserTest
import Parser.ModifierParserTest
import Parser.ModelParserTest
import Parser.ModifierBlockParserTest
import Parser.NewClassInstanceParserTest
import Parser.ObjectMethodCallParserTest
import Parser.PackageParserTest
import Parser.ParameterizedTypeParserTest
import Parser.ParameterParserTest
import Parser.ParenthesesParserTest
import Parser.ReassignParserTest
import Parser.StringLiteralParserTest
import Parser.ThisVarParserTest
import Parser.TypeParameterParserTest
import Parser.ValueTypeParserTest

parserTestList :: Test
parserTestList = TestList
    -- BaseParser
    [
        testSymbolSingle
      , testSymbolMultiple
      , testSymbolFail
      , testReservedWord
      , testIdentifier
      , testIdentifierFail
      , testFloat
      , testDouble
      , testInteger
      , testLong

    --, testAExprParserVar
    , testAExprParserInt

    -- BExprParser
    --, testBExprParserTrue
    --, testBExprParserFalse
    --, testBExprParserFail

    -- RExprParser
    --, testRExprParserGreaterVar
    --, testRExprParserLessVar
    --, testRExprParserGreaterEqualVar
    --, testRExprParserLessEqualVar
    --, testRExprParserGreaterInt
    --, testRExprParserLessInt
    --, testRExprParserGreaterEqualInt
    --, testRExprParserLessEqualInt

    -- ExprParser
    , testAnnotationParser
    --, testAnnotationParserLower

    --, testArgumentParserIdentifier
    --, testArgumentParserBoolTrue
    --, testArgumentParserBoolFalse

    --, testArgumentTypeParser
    --, testArgumentTypeParserReservedWord

    --, testArithmeticParserIdentifier
    --, testArithmeticParserClassVariable
    --, testArithmeticParserNewInstance
    --, testArithmeticParserMethodCall
    --, testArithmeticParserAdd
    --, testArithmeticParserSubtract
    --, testArithmeticParserMultiply
    --, testArithmeticParserDivide

    --, testAssignParserValWithType
    --, testAssignParserValWithoutType
    --, testAssignParserWithoutVal
    --, testAssignParserVarWithType
    --, testAssignParserVarWithoutType
    --, testAssignParserValWithParameterizedType
    --, testAssignParserVarWithParameterizedType

    --, testAssignParserTwoVars
    --, testAssignParserThreeVars
    --, testAssignParserFourVars

    --, testBooleanParserTrue
    --, testBooleanParserFalse
    --, testBooleanParserIdentifier
    --, testBooleanParserLessThanVar
    --, testBooleanParserLessThanInt
    --, testBooleanParserGreaterThanVar
    --, testBooleanParserGreaterThanInt
    --, testBooleanParserLessThanEqualVar
    --, testBooleanParserLessThanEqualInt
    --, testBooleanParserGreaterThanEqualVar
    --, testBooleanParserGreaterThanEqualInt

    --, testClassVariableParser

    --, testClassVariableParser
    --, testClassVariableParserUnderscores
    --, testClassVariableParserStartCapitals
    --, testClassVariableParserMissingVar
    --, testClassVariableParserMissingClassName

    , testForLoopGeneratorParser

    --, testIdentifierParserOneCharacter
    --, testIdentifierParserDigitFail
    --, testIdentifierParserContainsUnderscore
    --, testIdentifierParserContainsDigit
    --, testIdentifierParserStartsDigitFail
    --, testIdentifierParserCapital

    , testIfStmtParserBooleanTrue
    , testIfStmtParserBooleanFalse
    , testIfStmtParserElifTrue
    , testIfStmtParserElifFalse
    , testIfStmtParserElifElse
    , testIfStmtParserElse

    --, testImportParserSingle
    --, testImportParserEmptyFail
    --, testImportParserTwo
    --, testImportParserMultiple
    --, testImportParserStartsDigitFail
    --, testImportParserStartsDigitMultipleFail
    --, testImportParserCapital
    --, testImportParserUnderscore
    --, testImportParserMultipleUnderscore
    --, testImportParserContainsDigit

    --, testMethodCallParser

    , testMethodParserEmptyParams
    , testMethodParserMultipleParams
    --, testMethodParserMissingNameError
    --, testMethodParserMissingParens
    --, testMethodParserMissingName
    --, testMethodParserMissingReturnType
    , testMethodParserModifierPublic
    --, testMethodParserModifierProtected
    --, testMethodParserModifierPrivate
    --, testMethodParserModifierPublicAbstract
    --, testMethodParserModifierProtectedAbstract
    --, testMethodParserModifierPrivateAbstract
    --, testMethodParserModifierAbstract
    --, testMethodParserModifierPublicFinal
    --, testMethodParserModifierProtectedFinal
    --, testMethodParserModifierPrivateFinal
    --, testMethodParserModifierFinal
    --, testMethodParserModifierReordered1
    --, testMethodParserModifierReordered2
    --, testMethodParserModifierReordered3

    --, testAccessModifierParserPublic
    --, testAccessModifierParserProtected
    --, testAccessModifierParserPrivate
    --, testAbstractModifierParser
    --, testFinalModifierParser

      , testModelParser
      , testModelParserInner
    --, testModelParserClass
    --, testModelParserTrait
    --, testModelParserObject
    --, testModelParserParamsEmpty
    --, testModelParserParamsSingle
    --, testModelParserParamsMultiple
    --, testModelParserTypeParameter
    --, testModelParserTypeParameterExtends
    --, testModelParserTypeParameterExtendsImplements
    --, testModelParserTypeParameterImplements
    --, testModelParserTypeParameterImplementsMultiple
    --, testModelParserExtends
    --, testModelParserParentArgsEmpty
    --, testModelParserParentArgsSingle
    --, testModelParserParentArgsMultiple
    --, testModelParserImplements
    --, testModelParserImplementsMultiple
    --, testModelParserExtendsImplements
    --, testModelParserExtendsImplementsMultiple
    --, testModelParserImports
    --, testModelParserImportsFail
    --, testModelParserModifierBlock
    --, testModelParserConstructorBody
    --, testModelParserMethods

    --, testModelParserPublic
    --, testModelParserProtected
    --, testModelParserPrivate
    --, testModelParserPublicAbstract
    --, testModelParserPrivate
    --, testModelParserPrivateAbstract
    --, testModelParserAbstract
    --, testModelParserPublicFinal
    --, testModelParserProtectedFinal
    --, testModelParserPrivateFinal
    --, testModelParserFinal
    --, testModelParserReordered1
    --, testModelParserReordered2
    --, testModelParserReordered3

    --, testModelTypeParserClass
    --, testModelTypeParserObject
    --, testModelTypeParserTrait

    --, testModifierBlockParserPrivate
    --, testModifierBlockParserProtected
    --, testModifierBlockParserPublic
    --, testModifierBlockParserPrivateEmpty
    --, testModifierBlockParserProtectedEmpty
    --, testModifierBlockParserPublicEmpty

    --, testNewClassInstanceParserNoArgs
    --, testNewClassInstanceParserNoArgsUnderscore
    --, testNewClassInstanceParserNoArgsLowerCase
    --, testNewClassInstanceParserNewUpperCase
    --, testNewClassInstanceParserNoArgsNoParens
    --, testNewClassInstanceParserSingleArg
    --, testNewClassInstanceParserMultiArgs
    --, testNewClassInstanceParserMissingNew
    --, testNewClassInstanceParserMissingLeftParen
    --, testNewClassInstanceParserMissingRightParen

    --, testObjectMethodCallParserThis
    --, testObjectMethodCallParserObject
    --, testObjectMethodCallParserSuper

    --, testPackageParser

    --, testParameterParser
    --, testParameterParserMissingVar
    --, testParameterParserMissingType
    --, testParameterParserMissingColon

    --, testParameterizedTypeParser
    --, testParameterizedTypeParserLeftMissing
    --, testParameterizedTypeParserRightMissing
    --, testParameterizedTypeParserClassMissing

    --, testParenthesesParserVar
    --, testParenthesesParserNested
    --, testParenthesesParserNoOpenFail
    --, testParenthesesParserNoCloseFail

    --, testReassignParserObject
    --, testReassignParserArithmetic
    --, testReassignParserArithmeticTwoVars
    --, testReassignParserClassVar

    --, testStringLiteralSimple
    --, testStringLiteralSimpleWhitespace
    --, testStringLiteralEscapeTab
    --, testStringLiteralEmpty
    --, testStringLiteralNewLine
    --, testStringLiteralMultipleNewLine
    --, testStringLiteralUnescapedSingleQuote
    --, testStringLiteralEscapedSingleQuote
    --, testStringLiteralEscapedDoubleQuote
    --, testStringLiteralDoubleQuoteMultiple
    --, testStringLiteralUnfinishedFail
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

    --, testThisVarParserStartsDigitFail
    --, testThisVarParserContainsCapital
    --, testThisVarParserContainsDigit
    --, testThisVarParserContainsUnderscore
    --, testThisVarParserNotThisFail

    --, testTypeParameterParser
    --, testTypeParameterParserMissingLeft
    --, testTypeParameterParserMissingRight
    --, testTypeParameterParserMissingBoth

    --, testValueTypeParserOneCharacter
    --, testValueTypeParserDigitFail
    --, testValueTypeParserContainsUnderscore
    --, testValueTypeParserContainsDigit
    --, testValueTypeParserStartsDigitFail
    --, testValueTypeParserCapital
    ]
