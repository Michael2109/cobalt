module IntegrationTests where

import Test.HUnit

import Compiler.CompilerTest
import Compiler.PrintCompileTest
import JVM.AssemblerTest

integrationTestList :: Test
integrationTestList = do
    TestList [ "An example integration test" ~: exampleCompilerTest
             , "Print compile test"          ~: testPrintCompile
             , "Assembler Test"              ~: assemblerTest
             ]
