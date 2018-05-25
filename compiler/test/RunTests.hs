import Test.HUnit
import System.Exit
import System.Directory
import Control.Monad

import IntegrationTests
import Parser.ParserTests
import SymbolTable.SymbolTableTests
import TestUtil.CompilerTestUtil
import Util.UtilTests

main :: IO Counts
main = do

    let inputDir = "test/resources/"
    let outputDir = "cobalt_generated_classes/"
    outputDirExists <- doesDirectoryExist outputDir
    when outputDirExists $ removeDirectoryRecursive outputDir
    compileDirectory inputDir outputDir ""
    integrationTestResults <- runTestTT $ integrationTestList
    parserTestResults <- runTestTT $ parserTestList
    symbolTableTestResults <- runTestTT $ symbolTableTestList
    utilsTestResults <- runTestTT $ utilsTestList

    if errors integrationTestResults +
        failures integrationTestResults +
        errors parserTestResults +
        failures parserTestResults +
        errors symbolTableTestResults +
        failures symbolTableTestResults +
        errors utilsTestResults +
        failures utilsTestResults == 0
        then exitSuccess
        else exitFailure
