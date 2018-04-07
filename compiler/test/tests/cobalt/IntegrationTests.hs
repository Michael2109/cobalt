module IntegrationTests where

import Test.HUnit

import Compiler.CompilerTest
import JVM.AssemblerTest

integrationTestList :: Test
integrationTestList = do
    TestList [ "An example integration test" ~: exampleCompilerTest
             , "Assembler Test"              ~: assemblerTest
             ]
