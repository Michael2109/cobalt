
import Test.HUnit

import IntegrationTests
import ParserTests
import SymbolTableTests
import UtilsTests

main :: IO Counts
main = do
  runTestTT $ integrationTestList
  runTestTT $ parserTestList
  runTestTT $ symbolTableTestList
  runTestTT $ utilsTestList
