{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module AssemblerTest where

import Test.HUnit (Assertion, assertBool)
import System.IO
import System.Directory (removeFile, removeDirectory, doesFileExist, doesDirectoryExist, createDirectory)
import System.Environment (withArgs)
import Control.Monad
import Control.Monad.Exception
import qualified Data.ByteString.Lazy as B

import TestUtils

import CompilerExecutor

import JVM.ClassFile
import JVM.Converter
import JVM.Assembler
import JVM.Builder
import JVM.Exceptions
import JVM.Builder.Monad
import Java.ClassPath

import qualified Java.Lang
import qualified Java.IO

assemblerTest :: Assertion
assemblerTest = do

  generatedDirExists <- doesDirectoryExist generatedDirectory

  when (not $ generatedDirExists) $ createDirectory generatedDirectory

  testClass <- generateIO [] "Test" test
  let file = (generatedDirectory ++ "Test")
  B.writeFile (file ++ ".class") (encodeClass testClass)
  results <- executeBytecode "Test"
  removeFile (file ++ ".class")

  -- Return if there is any error
  assertBool "Incorrect output" (results == "Hello World\nArgument: 5\n")



test :: (Throws ENotFound e, Throws ENotLoaded e, Throws UnexpectedEndMethod e) => GenerateIO e ()
test = do

 -- withClassPath $ do
    -- Add current directory (with Hello.class) to ClassPath
  --  addDirectory generatedDirectory

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
      loadString "Hello World"
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
      --invokeStatic "Hello" helloJava
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
