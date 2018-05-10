module Compiler.CompilerTest where

import Test.HUnit (Assertion, assertBool)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory)
import System.Environment (withArgs)
import Control.Monad

import Compiler.CompilerExecutor

exampleCompilerTest :: Assertion
exampleCompilerTest = do

    -- Create string containing the code (If testing without a test .cobalt file)
    -- let code = "class ModuleName"

    let generatedDir = "cobalt_generated_classes/"
    let generatedInnerDir = generatedDir ++ "game/"

    generatedDirExists <- doesDirectoryExist generatedDir
    generatedInnerDirExists <- doesDirectoryExist generatedInnerDir

    when (not $ generatedDirExists) $ createDirectory generatedDir
    when (not $ generatedInnerDirExists) $ createDirectory generatedInnerDir

    -- Run the compiler with args
    withArgs ["-d", generatedDir, "-p", "test/resources/cobalt_src/", "game/Alien.cobalt"] execute

    -- Check the file has been compiled correctly
    fileExists <- doesFileExist "cobalt_generated_classes/game/Alien.class"

    assertBool "Check file has been generated" fileExists
