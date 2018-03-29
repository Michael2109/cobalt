module PackageParserTest where

import Test.HUnit

import Text.Megaparsec

import Block

import BaseParser
import ExprParser
import Parser


testPackageParser :: Test
testPackageParser = do
  let code = "package dir.sub_dir"
  TestCase $ assertEqual code
    (Package ["dir","sub_dir"])
    (case (parse (packageParser) "" code) of
      Left e -> Error
      Right x -> x)
