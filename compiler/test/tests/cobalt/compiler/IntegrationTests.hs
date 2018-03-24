module IntegrationTests where

import Test.HUnit

import CompilerTest
import IOUtils


integrationTestList :: Test
integrationTestList = do
  TestList [

       -- Compiler

       "An example integration test"   ~: exampleCompilerTest
       ]