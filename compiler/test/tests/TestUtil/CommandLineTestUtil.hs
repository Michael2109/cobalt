module TestUtil.CommandLineTestUtil where

import Test.HUnit

import Util.CommandLineUtil

--this approach has problems with with showing what test fails if stack trace is not printed
testCommandLineSuccess :: [String] -> ([CommandLineArgument],[FilePath]) -> Test
testCommandLineSuccess args result = do
    TestCase $ assertEqual (unwords args)
        (HValue result)
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))

testCommandLineFailure :: [String] -> Test
testCommandLineFailure args = do
    TestCase $ assertEqual (unwords args)
        (HError)
        (case (commandLineArgsInternal args) of
             Left  _ -> HError
             Right x -> (HValue x))


data TestHelper = HError
                | HValue ([CommandLineArgument],[FilePath])
                deriving (Show, Eq)
