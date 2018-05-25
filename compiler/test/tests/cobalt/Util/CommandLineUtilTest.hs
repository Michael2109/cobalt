module Util.CommandLineUtilTest where

import Test.HUnit

import Util.CommandLineUtil
import TestUtil.CommandLineTestUtil (testCommandLineFailure, testCommandLineSuccess)

testCommandLineOptionsEmpty :: Test
testCommandLineOptionsEmpty = testCommandLineSuccess [] ([],[])

testCommandLineOptionsSingleSourceFile :: Test
testCommandLineOptionsSingleSourceFile = testCommandLineSuccess ["source"] ([],["source"])

testCommandLineOptionsMultipleSources :: Test
testCommandLineOptionsMultipleSources = testCommandLineSuccess ["source1","source2","source3"] ([],["source1","source2","source3"])

testCommandLineOptionsHelpLower :: Test
testCommandLineOptionsHelpLower = testCommandLineSuccess ["-h"] ([Help],[])

testCommandLineOptionsHelpUpper :: Test
testCommandLineOptionsHelpUpper = testCommandLineSuccess ["-H"] ([Help],[])

testCommandLineOptionsHelpLong :: Test
testCommandLineOptionsHelpLong = testCommandLineSuccess ["--help"] ([Help],[])

testCommandLineOptionsVersionLong :: Test
testCommandLineOptionsVersionLong = testCommandLineSuccess ["--version"] ([Version],[])

testCommandLineOptionsVerboseLower :: Test
testCommandLineOptionsVerboseLower = testCommandLineSuccess ["-v"] ([VerboseMode],[])

testCommandLineOptionsVerboseUpper :: Test
testCommandLineOptionsVerboseUpper = testCommandLineSuccess ["-V"] ([VerboseMode],[])

testCommandLineOptionsVerboseLong1 :: Test
testCommandLineOptionsVerboseLong1 = testCommandLineSuccess ["--verbose"] ([VerboseMode],[])

testCommandLineOptionsVerboseLong2 :: Test
testCommandLineOptionsVerboseLong2 = testCommandLineSuccess ["--verbose-mode"] ([VerboseMode],[])

testCommandLineOptionsGenerateDebug :: Test
testCommandLineOptionsGenerateDebug = testCommandLineSuccess ["-g"] ([GenerateDebugSymbols],[])

testCommandLineOptionsGenerateDebugLong :: Test
testCommandLineOptionsGenerateDebugLong = testCommandLineSuccess ["--generate-debug"] ([GenerateDebugSymbols],[])

testCommandLineOptionsDebugModeLong :: Test
testCommandLineOptionsDebugModeLong = testCommandLineSuccess ["--debug-mode"] ([DebugMode],[])

testCommandLineOptionsMultipleShortSeparate :: Test
testCommandLineOptionsMultipleShortSeparate = testCommandLineSuccess ["-h", "-v", "-g"] ([Help, VerboseMode, GenerateDebugSymbols],[])

testCommandLineOptionsMultipleShortJoined :: Test
testCommandLineOptionsMultipleShortJoined = testCommandLineSuccess ["-hvg"] ([Help, VerboseMode, GenerateDebugSymbols],[])

testCommandLineOptionsMultipleShortHybrid :: Test
testCommandLineOptionsMultipleShortHybrid = testCommandLineSuccess ["-hv", "-g"] ([Help, VerboseMode, GenerateDebugSymbols],[])

testCommandLineOptionsSourceDirectoryShortWithSpace :: Test
testCommandLineOptionsSourceDirectoryShortWithSpace = testCommandLineSuccess ["-s", "DIR"] ([(SourceDir "DIR")],[])

testCommandLineOptionsSourceDirectoryShortNoSpace :: Test
testCommandLineOptionsSourceDirectoryShortNoSpace = testCommandLineSuccess ["-sDIR"] ([(SourceDir "DIR")],[])

testCommandLineOptionsSourceDirectoryLongWithSpace :: Test
testCommandLineOptionsSourceDirectoryLongWithSpace = testCommandLineSuccess ["--source-directory", "DIR"] ([(SourceDir "DIR")],[])

testCommandLineOptionsSourceDirectoryLongEqualSign :: Test
testCommandLineOptionsSourceDirectoryLongEqualSign = testCommandLineSuccess ["--source-directory=DIR"] ([(SourceDir "DIR")],[])

testCommandLineOptionsSourceDirectoryLongNoSpaceFail :: Test
testCommandLineOptionsSourceDirectoryLongNoSpaceFail = testCommandLineFailure ["--source-directoryDIR"]

testCommandLineOptionsSourceDirectoryShortNoArgumentFail :: Test
testCommandLineOptionsSourceDirectoryShortNoArgumentFail = testCommandLineFailure ["-s"]

testCommandLineOptionsSourceDirectoryLongNoArgumentFail :: Test
testCommandLineOptionsSourceDirectoryLongNoArgumentFail = testCommandLineFailure ["--source-directory"]

testCommandLineOptionsDestinationDirectoryShortWithSpace :: Test
testCommandLineOptionsDestinationDirectoryShortWithSpace = testCommandLineSuccess ["-d", "DIR"] ([(DestinationDir "DIR")],[])

testCommandLineOptionsDestinationDirectoryShortNoSpace :: Test
testCommandLineOptionsDestinationDirectoryShortNoSpace = testCommandLineSuccess ["-dDIR"] ([(DestinationDir "DIR")],[])

testCommandLineOptionsDestinationDirectoryLongWithSpace :: Test
testCommandLineOptionsDestinationDirectoryLongWithSpace = testCommandLineSuccess ["--destination-directory", "DIR"] ([(DestinationDir "DIR")],[])

testCommandLineOptionsDestinationDirectoryLongEqualSign :: Test
testCommandLineOptionsDestinationDirectoryLongEqualSign = testCommandLineSuccess ["--destination-directory=DIR"] ([(DestinationDir "DIR")],[])

testCommandLineOptionsDestinationDirectoryLongNoSpaceFail :: Test
testCommandLineOptionsDestinationDirectoryLongNoSpaceFail = testCommandLineFailure ["--destination-directoryDIR"]

testCommandLineOptionsDestinationDirectoryShortNoArgumentFail :: Test
testCommandLineOptionsDestinationDirectoryShortNoArgumentFail = testCommandLineFailure ["-d"]

testCommandLineOptionsDestinationDirectoryLongNoArgumentFail :: Test
testCommandLineOptionsDestinationDirectoryLongNoArgumentFail = testCommandLineFailure ["--destination-directory"]

testCommandLineOptionsClassPathShortWithSpace :: Test
testCommandLineOptionsClassPathShortWithSpace = testCommandLineSuccess ["-p", "DIR"] ([(ClassPath "DIR")],[])

testCommandLineOptionsClassPathShortNoSpace :: Test
testCommandLineOptionsClassPathShortNoSpace = testCommandLineSuccess ["-pDIR"] ([(ClassPath "DIR")],[])

testCommandLineOptionsClassPathLongWithSpace :: Test
testCommandLineOptionsClassPathLongWithSpace = testCommandLineSuccess ["--class-path", "DIR"] ([(ClassPath "DIR")],[])

testCommandLineOptionsClassPathLongEqualSign :: Test
testCommandLineOptionsClassPathLongEqualSign = testCommandLineSuccess ["--class-path=DIR"] ([(ClassPath "DIR")],[])

testCommandLineOptionsClassPathLongNoSpaceFail :: Test
testCommandLineOptionsClassPathLongNoSpaceFail = testCommandLineFailure ["--class-pathDIR"]

testCommandLineOptionsClassPathShortNoArgumentFail :: Test
testCommandLineOptionsClassPathShortNoArgumentFail = testCommandLineFailure ["-p"]

testCommandLineOptionsClassPathLongNoArgumentFail :: Test
testCommandLineOptionsClassPathLongNoArgumentFail = testCommandLineFailure ["--class-path"]

testCommandLineOptionsSourcesAfterSimpleOptions :: Test
testCommandLineOptionsSourcesAfterSimpleOptions = testCommandLineSuccess ["-h", "-v", "source1", "source2"] ([Help,VerboseMode],["source1", "source2"])

testCommandLineOptionsSimpleOptionsAfterSources :: Test
testCommandLineOptionsSimpleOptionsAfterSources = testCommandLineSuccess ["source1", "source2", "-h", "-v"] ([Help,VerboseMode],["source1", "source2"])

testCommandLineOptionsSimpleOptionsSourcesInterspersed :: Test
testCommandLineOptionsSimpleOptionsSourcesInterspersed = testCommandLineSuccess ["source1", "-h", "source2", "-v"] ([Help,VerboseMode],["source1", "source2"])

testCommandLineOptionsUnrecognizedFail :: Test
testCommandLineOptionsUnrecognizedFail = testCommandLineFailure ["-j"]

testCommandLineOptionsUnrecognizedLongFail :: Test
testCommandLineOptionsUnrecognizedLongFail = testCommandLineFailure ["--j"]

testCommandLineOptionsHybrid :: Test
testCommandLineOptionsHybrid = testCommandLineSuccess ["-h", "source1", "--class-path=DIR1", "-vg", "source2", "-d", "DIR2", "source3"]
    ([Help, (ClassPath "DIR1"), VerboseMode, GenerateDebugSymbols, (DestinationDir "DIR2")],["source1", "source2", "source3"])

testCommandLineOptionsExcessArgumentProvidedFail :: Test
testCommandLineOptionsExcessArgumentProvidedFail = testCommandLineFailure ["--help=DIR2"]
