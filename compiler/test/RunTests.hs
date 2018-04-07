import Test.HUnit
import System.Exit

import IntegrationTests
import Parsers.ParserTests
import SymbolTable.SymbolTableTests
import Utils.UtilsTests

main :: IO Counts
main = do

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
