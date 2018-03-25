module CompilerTest where

import Test.HUnit (Assertion, assertBool)
import System.IO
import System.Directory (removeFile, removeDirectory, doesFileExist, doesDirectoryExist, createDirectory)
import System.IO.Temp (createTempDirectory, writeTempFile)
import System.Environment (withArgs)
import Control.Monad

import CompilerExecutor

exampleCompilerTest :: Assertion
exampleCompilerTest = do

  -- Create string containing the code (If testing without a test .cobalt file)
  let code = "class ModuleName"

  let generatedDir = "cobalt_generated_classes/"
  let generatedInnerDir = generatedDir ++ "game/"

  generatedDirExists <- doesDirectoryExist generatedDir
  generatedInnerDirExists <- doesDirectoryExist generatedInnerDir

  when (not $ generatedDirExists) $ createDirectory generatedDir
  when (not $ generatedInnerDirExists) $ createDirectory generatedInnerDir

  -- Create a temporary directory
  tempDir <- createTempDirectory "" "cobalt_test"

  -- Write the file to a temporary location
  tempFile <- writeTempFile tempDir "FileName" code

  -- Run the compiler with args
  withArgs ["-d", generatedDir, "-p", "test/resources/cobalt_src/", "game/Alien.cobalt"] execute

  -- Check the file has been compiled correctly
  fileExists <- doesFileExist "cobalt_generated_classes/game/Alien.class"
  assertBool "Check file has been generated" fileExists

  -- Delete the generated file
  removeFile tempFile
  removeDirectory tempDir

  -- Return if there is any error
  assertBool "Error Message" (True == True)
