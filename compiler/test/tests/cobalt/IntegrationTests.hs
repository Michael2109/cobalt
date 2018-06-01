module IntegrationTests where

import Test.HUnit

import Compiler.CompilerTest
import Compiler.NumbersCompileTest
<<<<<<< Updated upstream
=======
import Compiler.ArithmeticCompileTest
import Compiler.StringLiteralsCompileTest
>>>>>>> Stashed changes
import Compiler.PrintCompileTest
import JVM.AssemblerTest

integrationTestList :: Test
integrationTestList = do
<<<<<<< Updated upstream
    TestList [ "An example integration test" ~: exampleCompilerTest
             , "Print compile test"          ~: testCompilePrint
             , "Numbers compile test"        ~: testCompileNumbers
             , "Assembler Test"              ~: assemblerTest
=======
    TestList [ "An example integration test"    ~: exampleCompilerTest
             , "Print compile test"             ~: testCompilePrint
             , "Numbers compile test"           ~: testCompileNumbers
             , "Arithmetic compile test"        ~: testCompileArithmetic
             , "String literals compile test"   ~: testCompileStringLiterals
             , "Assembler Test"                 ~: assemblerTest
>>>>>>> Stashed changes
             ]
