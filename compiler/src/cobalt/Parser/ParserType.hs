module Parser.ParserType where

import Text.Megaparsec
import Data.Void

type Parser = Parsec Void String