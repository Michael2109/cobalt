module IntegrationTests where

import Test.HUnit

import Compiler.CompilerTest
import Compiler.NumbersCompileTest
import Compiler.ArithmeticCompileTest
import Compiler.StringLiteralsCompileTest
import Compiler.PrintCompileTest
import JVM.AssemblerTest

integrationTestList :: Test
integrationTestList = do
    TestList [ "An example integration test"    ~: exampleCompilerTest
             , "Print compile test"             ~: testCompilePrint
             , "Numbers compile test"           ~: testCompileNumbers
             , "Arithmetic compile test"        ~: testCompileArithmetic
             , "String literals compile test"   ~: testCompileStringLiterals
             , "Assembler Test"                 ~: assemblerTest
             ]
