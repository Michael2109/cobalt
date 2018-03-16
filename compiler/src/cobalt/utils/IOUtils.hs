module IOUtils where

import Control.Monad
import System.FilePath.Posix
import System.Directory

allFilesIn :: FilePath -> IO [FilePath]
allFilesIn dir = getDirectoryContents dir

endsWith :: String -> FilePath -> Bool
endsWith extension file = takeExtension file == extension

cleanDir :: (FilePath -> Bool) -> String -> IO()
cleanDir extensionPredicate directory = do
  createDirectoryIfMissing True directory
  allFilesIn directory >>= mapM (\inputLoc ->
    if (endsWith "" inputLoc)
      then cleanDir extensionPredicate (directory ++ inputLoc ++ "/")
      else
        if(extensionPredicate inputLoc)
        then removeFile (directory ++ inputLoc)
        else return ()
    )
  return ()

-- | Traverse from 'top' directory and return all the files by
-- filtering out the 'exclude' predicate.
traverseDir :: FilePath -> FilePath -> IO [FilePath]
traverseDir inputDir top = do
  ds <- getDirectoryContents $ inputDir ++ top
  paths <- forM (ds) $ \d -> do
    let path = top </> d
    if takeExtension (inputDir ++ path) == ""
      then traverseDir (inputDir) path
      else return [path]
  return (concat paths)



generateMissingDirectories :: String -> String -> IO()
generateMissingDirectories inputDir outputDir = do
  allFilesIn inputDir >>= mapM (\inputLoc ->
    do
      createDirectoryIfMissing True outputDir
      when (takeExtension inputLoc == "") $ do
        generateMissingDirectories (inputDir ++ inputLoc ++ "/") (outputDir ++ inputLoc ++ "/")
      )
  return ()
