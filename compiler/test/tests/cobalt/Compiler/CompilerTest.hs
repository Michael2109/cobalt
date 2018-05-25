module Compiler.CompilerTest where

import Compiler.CompilerExecutor

import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (withArgs)
import Test.HUnit (Assertion, assertBool)

exampleCompilerTest :: Assertion
exampleCompilerTest = do

    let generatedDir = "cobalt_generated_classes/"
    let generatedInnerDir = generatedDir ++ "integration/"
    a <- getCurrentDirectory
    -- Run the compiler with args
    --withArgs ["-d", generatedInnerDir, "-p", "test/resources/integration/", "Example.cobalt"] execute
    withArgs ["-d", generatedInnerDir, "-p", "test/resources/", "-s", a] execute

    -- Check the file has been compiled correctly
    fileExists <- doesFileExist $ generatedInnerDir ++ "Example.class"

    assertBool "Class file isn't generated" fileExists
