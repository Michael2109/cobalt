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
  [ Option ['d']     ["destination-directory"]  (ReqArg DestinationDir "<directory>") "Specify where to place generated class files"
  , Option ['p']     ["class-path"]             (ReqArg ClassPath "<directory>")      "Specify where to find user class files and annotation processors"
  , Option ['h','H'] ["help"]                   (NoArg Help)                          "Print a synopsis of standard options"
  , Option []        ["version"]                (NoArg Version)                       "Version information"
  , Option ['v','V'] ["verbose","verbose-mode"] (NoArg VerboseMode)                   "Output messages about what the compiler is doing"
  , Option ['g']     ["generate-debug"]         (NoArg GenerateDebugSymbols)          "Generate debugging information to resulting bytecode"
  , Option []        ["debug-mode"]             (NoArg DebugMode)                     "Run compiler in debug mode"
  ]

helpInfo :: String
helpInfo = usageInfo header commandLineOptions
  -- In fact, the options work correctly even if they are interleaved with source files.
  -- The order is not enforced (to do so, simply replace Permute with RequireOrder in flags)
  -- because it would cause problems with checking wether there are some trailing options
  -- (they would be parsed as source files which might cause unintuitive exceptions)
  -- javac does it this way too.
  where header = "Usage: compiler-exec <options> <source files>\nwhere possible options include:"


commandLineArgsInternal :: [String] -> (Either [String] ([CommandLineArgument],[FilePath]))
commandLineArgsInternal args =
  case getOpt Permute commandLineOptions args of
    (f,d,[]  ) -> Right (f,d) -- contents of d are arguments not options
    (_,_,errors) -> Left errors


commandLineArgs :: [String] -> IO([CommandLineArgument],[FilePath])
commandLineArgs args =
  case (commandLineArgsInternal args) of
    (Right x) -> return x  -- contents of d are arguments not options
    (Left errors) -> raiseErrorsException errors


raiseErrorsException :: [String] -> IO([CommandLineArgument],[FilePath])
raiseErrorsException errors = ioError $ userError $ concat ("\n":errors) ++ ('\n':helpInfo)
