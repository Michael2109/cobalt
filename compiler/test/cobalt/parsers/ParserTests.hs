
import Test.HUnit

import ParserTests
import SymbolTableTests

main :: IO Counts
main = do
  runTestTT $ parserTestList
  runTestTT $ symbolTableTestList
