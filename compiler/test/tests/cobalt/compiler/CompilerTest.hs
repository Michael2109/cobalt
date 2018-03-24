module CompilerTest where

import Test.HUnit (Assertion, assertBool)
import System.IO
import System.Directory (removeFile, removeDirectory, doesFileExist, doesDirectoryExist)
import System.IO.Temp (createTempDirectory, writeTempFile)
import System.Environment (withArgs)

import CompilerExecutor

exampleCompilerTest :: Assertion
exampleCompilerTest = do

  -- Create string containing the code (If testing without a test .cobalt file)
  let code = "class ModuleName"

  -- Create a temporary directory
  tempDir <- createTempDirectory "" "cobalt_test"

  -- Write the file to a temporary location
  tempFile <- writeTempFile tempDir "FileName" code

  -- Run the compiler with args
  withArgs ["-d", "cobalt_generated_classes/", "-p", "test/resources/cobalt_src/", "game/Alien.cobalt"] execute

  -- Check the file has been compiled correctly
  fileExists <- doesFileExist "cobalt_generated_classes/game/Alien.class"
  assertBool "Check file has been generated" fileExists

  -- Delete the generated file
  removeFile tempFile
  removeDirectory tempDir

  -- Return if there is any error
  assertBool "Error Message" (True == True)
