{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

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

  --testClass <- generateIO [] "Test" test
  --B.writeFile "Test.class" (encodeClass testClass)

  -- Return if there is any error
  assertBool "Error Message" (True == True)



test :: (Throws ENotFound e, Throws ENotLoaded e, Throws UnexpectedEndMethod e) => GenerateIO e ()
test = do
  withClassPath $ do
      -- Add current directory (with Hello.class) to ClassPath
      addDirectory "."

  -- Load method signature: Hello.hello() from Hello.class
  helloJava <- getClassMethod "./Hello" "hello"

  -- Initializer method. Just calls java.lang.Object.<init>
  newMethod [ACC_PUBLIC] "<init>" [] ReturnsVoid $ do
      setStackSize 1

      aload_ I0
      invokeSpecial Java.Lang.object Java.Lang.objectInit
      i0 RETURN

  -- Declare hello() method and bind it's signature to hello.
  hello <- newMethod [ACC_PUBLIC, ACC_STATIC] "hello" [IntType] ReturnsVoid $ do
      setStackSize 8

      getStaticField Java.Lang.system Java.IO.out
      loadString "Здравствуй, мир!"
      invokeVirtual Java.IO.printStream Java.IO.println
      getStaticField Java.Lang.system Java.IO.out
      loadString "Argument: %d\n"
      iconst_1
      allocArray Java.Lang.object
      dup
      iconst_0
      iload_ I0
      invokeStatic Java.Lang.integer Java.Lang.valueOfInteger
      aastore
      invokeVirtual Java.IO.printStream Java.IO.printf
      -- Call Hello.hello()
      invokeStatic "Hello" helloJava
      pop
      i0 RETURN

  -- Main class method.
  newMethod [ACC_PUBLIC, ACC_STATIC] "main" [arrayOf Java.Lang.stringClass] ReturnsVoid $ do
      setStackSize 1

      iconst_5
      -- Call previously declared method
      invokeStatic "Test" hello
      i0 RETURN

  return ()