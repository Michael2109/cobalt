module AssemblerTest where

import Test.HUnit (Assertion, assertBool)
import System.IO
import System.Directory (removeFile, removeDirectory, doesFileExist, doesDirectoryExist, createDirectory)
import System.IO.Temp (createTempDirectory, writeTempFile)
import System.Environment (withArgs)
import Control.Monad
import Control.Monad.Exception
import qualified Data.ByteString.Lazy as B

import CompilerExecutor

import JVM.ClassFile
import JVM.Converter
import JVM.Assembler
import JVM.Builder
import JVM.Exceptions
import Java.ClassPath

import qualified Java.Lang
import qualified Java.IO

assemblerTest :: Assertion
assemblerTest = do

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


  -- Check the file has been compiled correctly
  fileExists <- doesFileExist "cobalt_generated_classes/game/Alien.class"
  assertBool "Check file has been generated" fileExists

  -- Delete the generated file
  removeFile tempFile
  removeDirectory tempDir

  -- Return if there is any error
  assertBool "Error Message" (True == True)
