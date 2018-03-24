module CommandLineUtils where

import System.Console.GetOpt

data CommandLineArgument = ClassPath FilePath
          | DestinationDir FilePath
          | Version
          | Help
          | DebugMode
          | GenerateDebugSymbols
          | VerboseMode
          deriving (Show, Eq)

commandLineOptions :: [OptDescr CommandLineArgument]
commandLineOptions =
  [ Option ['d']     ["destination-directory"]  (ReqArg DestinationDir "DIR")   "destination DIR"
  , Option ['p']     ["class-path"]             (ReqArg ClassPath "DIR")        "classpath DIR"
  , Option ['h','H'] ["help"]                   (NoArg Help)                    "show help message"
  , Option []        ["version"]                (NoArg Version)                 "show version info"
  , Option ['v','V'] ["verbose","verbose-mode"] (NoArg VerboseMode)             "run compiler in verbose mode"
  , Option ['g']     ["generate-debug"]         (NoArg GenerateDebugSymbols)    "generate debugging information to resulting bytecode"
  , Option []        ["debug-mode"]             (NoArg DebugMode)               "run compiler in debug mode"
  ]

helpInfo :: String
helpInfo = usageInfo header commandLineOptions
  where header = "Usage: compiler-exec [OPTIONS]... FILE [FILE]..."

flags :: [String] -> IO([CommandLineArgument],[FilePath])
flags args =
  case getOpt Permute commandLineOptions args of
    (f,d,[]  ) -> return (f,d) -- contents of d are arguments not options
    (_,_,errors) -> raiseErrorsException errors

raiseErrorsException :: [String] -> IO([CommandLineArgument],[FilePath])
raiseErrorsException errors = ioError $ userError $ concat ("\n":errors) ++ ('\n':helpInfo)
