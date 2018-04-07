module Util.CommandLineUtilTest where

import Test.HUnit

import Util.CommandLineUtil

data TestHelper = HError
                | HValue ([CommandLineArgument],[FilePath])
                deriving (Show, Eq)

testCommandLineOptionsEmpty :: Test
testCommandLineOptionsEmpty = do
    let args = []
    TestCase $ assertEqual args
        (HValue ([],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsSingleSourceFile :: Test
testCommandLineOptionsSingleSourceFile = do
    let args = ["source"]
    TestCase $ assertEqual (unwords args)
        (HValue ([],["source"]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsMultipleSources :: Test
testCommandLineOptionsMultipleSources = do
    let args = ["source1","source2","source3"]
    TestCase $ assertEqual (unwords args)
        (HValue ([],["source1","source2","source3"]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsHelpLower :: Test
testCommandLineOptionsHelpLower = do
    let args = ["-h"]
    TestCase $ assertEqual (unwords args)
        (HValue ([Help],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsHelpUpper :: Test
testCommandLineOptionsHelpUpper = do
    let args = ["-H"]
    TestCase $ assertEqual (unwords args)
        (HValue ([Help],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsHelpLong :: Test
testCommandLineOptionsHelpLong = do
    let args = ["--help"]
    TestCase $ assertEqual (unwords args)
        (HValue ([Help],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsVersionLong :: Test
testCommandLineOptionsVersionLong = do
    let args = ["--version"]
    TestCase $ assertEqual (unwords args)
        (HValue ([Version],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsVerboseLower :: Test
testCommandLineOptionsVerboseLower = do
    let args = ["-v"]
    TestCase $ assertEqual (unwords args)
        (HValue ([VerboseMode],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsVerboseUpper :: Test
testCommandLineOptionsVerboseUpper = do
    let args = ["-V"]
    TestCase $ assertEqual (unwords args)
        (HValue ([VerboseMode],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsVerboseLong1 :: Test
testCommandLineOptionsVerboseLong1 = do
    let args = ["--verbose"]
    TestCase $ assertEqual (unwords args)
        (HValue ([VerboseMode],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsVerboseLong2 :: Test
testCommandLineOptionsVerboseLong2 = do
    let args = ["--verbose-mode"]
    TestCase $ assertEqual (unwords args)
        (HValue ([VerboseMode],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsGenerateDebug :: Test
testCommandLineOptionsGenerateDebug = do
    let args = ["-g"]
    TestCase $ assertEqual (unwords args)
        (HValue ([GenerateDebugSymbols],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsGenerateDebugLong :: Test
testCommandLineOptionsGenerateDebugLong = do
    let args = ["--generate-debug"]
    TestCase $ assertEqual (unwords args)
        (HValue ([GenerateDebugSymbols],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsDebugModeLong :: Test
testCommandLineOptionsDebugModeLong = do
    let args = ["--debug-mode"]
    TestCase $ assertEqual (unwords args)
        (HValue ([DebugMode],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsMultipleShortSeparate :: Test
testCommandLineOptionsMultipleShortSeparate = do
    let args = ["-h", "-v", "-g"]
    TestCase $ assertEqual (unwords args)
        (HValue ([Help, VerboseMode, GenerateDebugSymbols],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsMultipleShortJoined :: Test
testCommandLineOptionsMultipleShortJoined = do
    let args = ["-hvg"]
    TestCase $ assertEqual (unwords args)
        (HValue ([Help, VerboseMode, GenerateDebugSymbols],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsMultipleShortHybrid :: Test
testCommandLineOptionsMultipleShortHybrid = do
    let args = ["-hv", "-g"]
    TestCase $ assertEqual (unwords args)
        (HValue ([Help, VerboseMode, GenerateDebugSymbols],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsDestinationDirectoryShortWithSpace :: Test
testCommandLineOptionsDestinationDirectoryShortWithSpace = do
    let args = ["-d", "DIR"]
    TestCase $ assertEqual (unwords args)
        (HValue ([(DestinationDir "DIR")],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsDestinationDirectoryShortNoSpace :: Test
testCommandLineOptionsDestinationDirectoryShortNoSpace = do
    let args = ["-dDIR"]
    TestCase $ assertEqual (unwords args)
        (HValue ([(DestinationDir "DIR")],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsDestinationDirectoryLongWithSpace :: Test
testCommandLineOptionsDestinationDirectoryLongWithSpace = do
    let args = ["--destination-directory", "DIR"]
    TestCase $ assertEqual (unwords args)
        (HValue ([(DestinationDir "DIR")],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsDestinationDirectoryLongEqualSign :: Test
testCommandLineOptionsDestinationDirectoryLongEqualSign = do
    let args = ["--destination-directory=DIR"]
    TestCase $ assertEqual (unwords args)
        (HValue ([(DestinationDir "DIR")],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsDestinationDirectoryLongNoSpaceFail :: Test
testCommandLineOptionsDestinationDirectoryLongNoSpaceFail = do
    let args = ["--destination-directoryDIR"]
    TestCase $ assertEqual (unwords args)
        (HError)
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsDestinationDirectoryShortNoArgumentFail :: Test
testCommandLineOptionsDestinationDirectoryShortNoArgumentFail = do
    let args = ["-d"]
    TestCase $ assertEqual (unwords args)
        (HError)
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsDestinationDirectoryLongNoArgumentFail :: Test
testCommandLineOptionsDestinationDirectoryLongNoArgumentFail = do
    let args = ["--destination-directory"]
    TestCase $ assertEqual (unwords args)
        (HError)
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsClassPathShortWithSpace :: Test
testCommandLineOptionsClassPathShortWithSpace = do
    let args = ["-p", "DIR"]
    TestCase $ assertEqual (unwords args)
        (HValue ([(ClassPath "DIR")],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsClassPathShortNoSpace :: Test
testCommandLineOptionsClassPathShortNoSpace = do
    let args = ["-pDIR"]
    TestCase $ assertEqual (unwords args)
        (HValue ([(ClassPath "DIR")],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsClassPathLongWithSpace :: Test
testCommandLineOptionsClassPathLongWithSpace = do
    let args = ["--class-path", "DIR"]
    TestCase $ assertEqual (unwords args)
        (HValue ([(ClassPath "DIR")],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsClassPathLongEqualSign :: Test
testCommandLineOptionsClassPathLongEqualSign = do
    let args = ["--class-path=DIR"]
    TestCase $ assertEqual (unwords args)
        (HValue ([(ClassPath "DIR")],[]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsClassPathLongNoSpaceFail :: Test
testCommandLineOptionsClassPathLongNoSpaceFail = do
    let args = ["--class-pathDIR"]
    TestCase $ assertEqual (unwords args)
        (HError)
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsClassPathShortNoArgumentFail :: Test
testCommandLineOptionsClassPathShortNoArgumentFail = do
    let args = ["-p"]
    TestCase $ assertEqual (unwords args)
        (HError)
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsClassPathLongNoArgumentFail :: Test
testCommandLineOptionsClassPathLongNoArgumentFail = do
    let args = ["--class-path"]
    TestCase $ assertEqual (unwords args)
        (HError)
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsSourcesAfterSimpleOptions :: Test
testCommandLineOptionsSourcesAfterSimpleOptions = do
    let args = ["-h", "-v", "source1", "source2"]
    TestCase $ assertEqual (unwords args)
        (HValue ([Help,VerboseMode],["source1", "source2"]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsSimpleOptionsAfterSources :: Test
testCommandLineOptionsSimpleOptionsAfterSources = do
    let args = ["source1", "source2", "-h", "-v"]
    TestCase $ assertEqual (unwords args)
        (HValue ([Help,VerboseMode],["source1", "source2"]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsSimpleOptionsSourcesInterspersed :: Test
testCommandLineOptionsSimpleOptionsSourcesInterspersed = do
    let args = ["source1", "-h", "source2", "-v"]
    TestCase $ assertEqual (unwords args)
        (HValue ([Help,VerboseMode],["source1", "source2"]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsUnrecognizedFail :: Test
testCommandLineOptionsUnrecognizedFail = do
    let args = ["-j"]
    TestCase $ assertEqual (unwords args)
        (HError)
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsUnrecognizedLongFail :: Test
testCommandLineOptionsUnrecognizedLongFail = do
    let args = ["--j"]
    TestCase $ assertEqual (unwords args)
        (HError)
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsHybrid :: Test
testCommandLineOptionsHybrid = do
    let args = ["-h", "source1", "--class-path=DIR1", "-vg", "source2", "-d", "DIR2", "source3"]
    TestCase $ assertEqual (unwords args)
        (HValue ([Help, (ClassPath "DIR1"), VerboseMode, GenerateDebugSymbols, (DestinationDir "DIR2")],["source1", "source2", "source3"]))
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineOptionsExcessArgumentProvidedFail :: Test
testCommandLineOptionsExcessArgumentProvidedFail = do
    let args = ["--help=DIR2"]
    TestCase $ assertEqual (unwords args)
        (HError)
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))
