module CompilerTest where

import Test.HUnit (Assertion, assertBool)
import System.IO
import System.Directory
import System.IO.Temp (createTempDirectory, writeTempFile)
import System.Environment (withArgs)

import CompilerExecutor

testCompiler :: Assertion
testCompiler = do

  -- Create string containing the code
  let code = "class ModuleName"

  -- Create a temporary directory
  tempDir <- createTempDirectory "" "cobalt_test"

  print $ "TempDir: " ++ tempDir

  -- Write the file to a temporary location
  tempFile <- writeTempFile tempDir "FileNameTemplate" code

  print $ "TempFile: " ++ tempFile

  -- Run the compiler with args
  withArgs ["-d", "cobalt_generated_classes", "-p", "test/resources/cobalt_src/", "test/resources/cobalt_src/game/Alien.cobalt"] execute

  -- Check compiled file

  -- Delete the generated file
  removeFile tempFile
  removeDirectory tempDir


  assertBool "Error Message" (("x") == (""))
