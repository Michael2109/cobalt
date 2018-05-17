module TestUtil.CompilerTestUtil where

import System.Directory
import System.FilePath.Posix
import System.Environment (withArgs)

import Compiler.CompilerExecutor

allFilesIn dir = getDirectoryContents dir

compileDirectory :: FilePath -> FilePath -> FilePath -> IO ()
compileDirectory classPath outputDir currentDir = do
    putStrLn $ show [classPath, outputDir, currentDir]
    allFilesIn (classPath ++ currentDir) >>= mapM (\inputLoc ->
        if (takeExtension inputLoc == "")
        then compileDirectory classPath outputDir (currentDir ++ inputLoc ++ "/")
        else
            if(takeExtension inputLoc == ".cobalt")
            then do
                putStrLn (classPath ++ " : " ++ outputDir ++ " : " ++ currentDir ++ inputLoc)
                createDirectoryIfMissing True (outputDir ++ currentDir ++ "/")
                withArgs ["-d", outputDir, "-p", classPath, currentDir ++ inputLoc] execute
            else putStrLn ""
        )

    putStrLn "Complete"
