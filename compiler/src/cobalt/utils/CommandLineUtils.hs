module CommandLineUtils where

import System.Console.GetOpt

data CommandLineArgument = ClassPath String
          | DestinationDir String
          | FileToCompile  String
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

flags :: [String] -> IO([CommandLineArgument],[String])
flags args =
  case getOpt Permute commandLineOptions args of
    (f,d,[]  ) -> return (f,d) -- contents of d are arguments not options
    (_,_,errors) -> raiseErrorsException errors

commandLineArgs :: [String] -> IO([CommandLineArgument])
commandLineArgs args = do
  (options, arguments) <- flags args
  return (options ++ (map FileToCompile arguments))

raiseErrorsException :: [String] -> IO([CommandLineArgument],[String])
raiseErrorsException errors = ioError $ userError $ concat ("\n":errors) ++ ('\n':helpInfo)
