{-|
Module      : CompilerExecutor
Description : Calls all of the functions to compile and generate the code.
-}
module CompilerExecutor where

import System.Environment
import Control.Monad

import Compiler
import SymbolTable
import CommandLineUtils
import IOUtils
import Utils

execute :: IO ()
execute = do
  args <- getArgs
  let isFileToCompile (FileToCompile _ ) = True
      isFileToCompile _ = False
  let isClassPath (ClassPath _ ) = True
      isClassPath _ = False
  let isDestinationDir (DestinationDir _ ) = True
      isDestinationDir _ = False

  options <- commandLineArgs args

  let isHelpPresent = elem (Help) options
  let isVersionPresent = elem (Version) options
  let inputFiles = map (\(FileToCompile x) -> x) (filter isFileToCompile options)

  -- if multiple class paths or destination dirs are provided, first is taken instead of throwing error
  -- Note: javac takes the last and does not produce error either
  -- classpath is set to "cobalt_generated_java/" temporarly until we stop compiling to java. Original default was "./"
  let classOutputDir = (\(DestinationDir dd) -> dd) . (defaultHead (DestinationDir "cobalt_generated_classes/")) $ filter isDestinationDir options
  when isVersionPresent printVersion
  when isHelpPresent printHelpInfo

  -- in case of using help option the compiler itself is not run, only appropiate message is produced
  if isHelpPresent
    then return ()
    else
      if (inputFiles == [])
        then do
          _ <- raiseErrorsException ["no source files specified\n"]
          return ()
        else do
          cleanDir (endsWith ".class") classOutputDir
          putStrLn "Compiling - Cobalt -> Bytecode"
          compile inputFiles classOutputDir
          putStrLn "Complete."
