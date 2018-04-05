module IntegrationTests where

import Test.HUnit

import CompilerTest
import AssemblerTest
import IOUtils

integrationTestList :: Test
integrationTestList = do
    TestList [ "An example integration test" ~: exampleCompilerTest
             , "Assembler Test"              ~: assemblerTest
             ]
