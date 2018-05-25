{-|
Module      : CompilerExecutor
Description : Calls all of the functions to compile and generate the code.
-}
module Compiler.CompilerExecutor where

import Control.Monad
import System.Environment

import Compiler.Compiler
import Util.CommandLineUtil (commandLineArgs, CommandLineArgument (..), raiseErrorsException )
import Util.IOUtil
import Util.GeneralUtil
import SymbolTable.SymbolTable

import System.Directory
import System.FilePath.Posix
import System.Environment (withArgs)


allFilesIn dir = getDirectoryContents dir

flattenDirectory :: FilePath -> IO ([FilePath])
flattenDirectory dir= do
    contents <- listDirectory formattedDir
    flattened <- mapM recurse contents
    return $ concat flattened
  where
      formattedDir = if last dir == '/' then dir else dir ++ "/" --sanity check for dir == "" TODO
      recurse inputLoc = if (takeExtension inputLoc == ".cobalt")
                           then return $ [formattedDir ++ inputLoc]
                           else do
                               isDirectory <- doesDirectoryExist (formattedDir ++ inputLoc)
                               if isDirectory
                               then flattenDirectory formattedDir
                               else return $ []

execute :: IO ()
execute = do
    args <- getArgs

    let isClassPath (ClassPath _ ) = True
        isClassPath _ = False
    let isDestinationDir (DestinationDir _ ) = True
        isDestinationDir _ = False
    let isSourceDir (SourceDir _ ) = True
        isSourceDir _ = False

    (options,filesToCompile) <- commandLineArgs args

    let helpPresent = elem (Help) options
    let versionPresent = elem (Version) options
    let debugModeOn = elem (DebugMode) options
    let verboseModeOn = elem (VerboseMode) options
    let generateDebugSymbolsOn = elem (GenerateDebugSymbols) options
    let dirsToCompile = map (\(SourceDir x)->x) $ filter isSourceDir options
    -- if multiple class paths or destination dirs are provided, first is taken instead of throwing error
    -- Note: javac takes the last and does not produce error either
    -- classpath is set to "cobalt_generated_java/" temporarly until we stop compiling to java. Original default was "./"
    let classPath = (\(ClassPath cp) -> cp) . (defaultHead (ClassPath "./")) $ filter isClassPath options
    let classOutputDir = (\(DestinationDir dd) -> dd) . (defaultHead (DestinationDir "./")) $ filter isDestinationDir options

    when debugModeOn (putStrLn "Running compiler in debug mode")
    when versionPresent printVersion
    when helpPresent printHelpInfo

    -- in case of using help option the compiler itself is not run, only appropiate message is produced
    if helpPresent
        then return ()
        else
            if (filesToCompile == [] && dirsToCompile == [])
                then do
                    _ <- raiseErrorsException ["No source files specified\n"]
                    return ()
                else do --construct the directory hierarchy here
                    flattenedFiles <- mapM flattenDirectory dirsToCompile
                    let files = filesToCompile ++ (concat flattenedFiles)
                    compile options classPath files classOutputDir
