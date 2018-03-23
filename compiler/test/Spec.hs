
import Test.HUnit

import IntegrationTests
import ParserTests
import SymbolTableTests

main :: IO Counts
main = do
  runTestTT $ integrationTestList
  runTestTT $ parserTestList
  runTestTT $ symbolTableTestList
