module IntegrationTests where

import Test.HUnit

import CompilerTest


integrationTestList :: Test
integrationTestList = do
  TestList [

       -- Compiler

       "An example integration test"   ~: exampleCompilerTest
       ]