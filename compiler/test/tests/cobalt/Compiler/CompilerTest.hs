module Compiler.CompilerTest where

import Test.HUnit (Assertion, assertBool)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory)
import System.Environment (withArgs)
import Control.Monad

import Compiler.CompilerExecutor

exampleCompilerTest :: Assertion
exampleCompilerTest = do

    let generatedDir = "cobalt_generated_classes/"
    let generatedInnerDir = generatedDir ++ "integration/"

    -- Run the compiler with args
    --withArgs ["-d", generatedInnerDir, "-p", "test/resources/integration/", "Example.cobalt"] execute

    -- Check the file has been compiled correctly
    fileExists <- doesFileExist $ generatedInnerDir ++ "Example.class"

    assertBool "Class file isn't generated" fileExists
