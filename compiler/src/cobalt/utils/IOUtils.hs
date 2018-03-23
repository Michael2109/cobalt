module IOUtils where

import Control.Monad
import System.FilePath.Posix
import System.Directory
import System.Console.GetOpt


data CommandlineArgument = ClassPath String
          | DestinationDir String
          | FileToCompile  String
          | Version
          | Help
          deriving (Show, Eq)

commandlineOptions :: [OptDescr CommandlineArgument]
commandlineOptions =
  [ Option ['d']     ["destination-directory"]  (ReqArg DestinationDir "DIR")   "destination DIR"
  , Option ['p']     ["class-path"]             (ReqArg ClassPath "DIR")        "classpath DIR"
  , Option ['h','H'] ["help"]                   (NoArg Help)                    "show help message"
  , Option ['v','V'] ["version"]                (NoArg Version)                 "show version info"
  ]

flags :: [String] -> IO([CommandlineArgument],[String])
flags args =
  case getOpt Permute commandlineOptions args of
    (f,d,[]  ) -> return (f,d) -- contents of d are arguments not options
    (_,_,errors) -> ioError $ userError $ concat errors ++ usageInfo header commandlineOptions
    where header = "Usage: compiler-exec [OPTIONS]... FILE [FILE]..."

commandlineArgs :: [String] -> IO([CommandlineArgument])
commandlineArgs args = do
  (options, arguments) <- flags args
  return (options ++ (map FileToCompile arguments))

defaultHead :: a -> [a] -> a
defaultHead x [] = x
defaultHead _ xs = head xs

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
    if endsWith "" (inputDir ++ path)
      then traverseDir (inputDir) path
      else return [path]
  return (concat paths)

generateMissingDirectories :: String -> String -> IO()
generateMissingDirectories inputDir outputDir = do
  allFilesIn inputDir >>= mapM (\inputLoc ->
    do
      createDirectoryIfMissing True outputDir
      when (endsWith "" inputLoc) $ do
        generateMissingDirectories (inputDir ++ inputLoc ++ "/") (outputDir ++ inputLoc ++ "/")
      )
  return ()
