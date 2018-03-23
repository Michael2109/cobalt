module CommandLineUtils where

import System.Console.GetOpt

data CommandLineArgument = ClassPath String
          | DestinationDir String
          | FileToCompile  String
          | Version
          | Help
          deriving (Show, Eq)

commandLineOptions :: [OptDescr CommandLineArgument]
commandLineOptions =
  [ Option ['d']     ["destination-directory"]  (ReqArg DestinationDir "DIR")   "destination DIR"
  , Option ['p']     ["class-path"]             (ReqArg ClassPath "DIR")        "classpath DIR"
  , Option ['h','H'] ["help"]                   (NoArg Help)                    "show help message"
  , Option ['v','V'] ["version"]                (NoArg Version)                 "show version info"
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
