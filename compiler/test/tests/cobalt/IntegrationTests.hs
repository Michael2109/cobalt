module IntegrationTests where

import Test.HUnit

import Compiler.CompilerTest
import Compiler.NumbersCompileTest
import Compiler.PrintCompileTest
import JVM.AssemblerTest

integrationTestList :: Test
integrationTestList = do
    TestList [ "An example integration test" ~: exampleCompilerTest
             , "Print compile test"          ~: testCompilePrint
             , "Numbers compile test"        ~: testCompileNumbers
             , "Assembler Test"              ~: assemblerTest
             ]
