module Util.UtilTests where

import Test.HUnit

import Util.CommandLineUtilTest

utilsTestList :: Test
utilsTestList = TestList
    [ testCommandLineOptionsEmpty
    , testCommandLineOptionsSingleSourceFile
    , testCommandLineOptionsMultipleSources
    , testCommandLineOptionsHelpLower
    , testCommandLineOptionsHelpUpper
    , testCommandLineOptionsHelpLong
    , testCommandLineOptionsVersionLong
    , testCommandLineOptionsVerboseLower
    , testCommandLineOptionsVerboseUpper
    , testCommandLineOptionsVerboseLong1
    , testCommandLineOptionsVerboseLong2
    , testCommandLineOptionsGenerateDebug
    , testCommandLineOptionsGenerateDebugLong
    , testCommandLineOptionsDebugModeLong
    , testCommandLineOptionsMultipleShortSeparate
    , testCommandLineOptionsMultipleShortJoined
    , testCommandLineOptionsMultipleShortHybrid
    , testCommandLineOptionsDestinationDirectoryShortWithSpace
    , testCommandLineOptionsDestinationDirectoryShortNoSpace
    , testCommandLineOptionsDestinationDirectoryLongWithSpace
    , testCommandLineOptionsDestinationDirectoryLongEqualSign
    , testCommandLineOptionsDestinationDirectoryLongNoSpaceFail
    , testCommandLineOptionsDestinationDirectoryShortNoArgumentFail
    , testCommandLineOptionsDestinationDirectoryLongNoArgumentFail
    , testCommandLineOptionsClassPathShortWithSpace
    , testCommandLineOptionsClassPathShortNoSpace
    , testCommandLineOptionsClassPathLongWithSpace
    , testCommandLineOptionsClassPathLongEqualSign
    , testCommandLineOptionsClassPathLongNoSpaceFail
    , testCommandLineOptionsClassPathShortNoArgumentFail
    , testCommandLineOptionsClassPathLongNoArgumentFail
    , testCommandLineOptionsSourcesAfterSimpleOptions
    , testCommandLineOptionsSimpleOptionsAfterSources
    , testCommandLineOptionsSimpleOptionsSourcesInterspersed
    , testCommandLineOptionsUnrecognizedFail
    , testCommandLineOptionsUnrecognizedLongFail
    , testCommandLineOptionsHybrid
    , testCommandLineOptionsExcessArgumentProvidedFail
    ]
