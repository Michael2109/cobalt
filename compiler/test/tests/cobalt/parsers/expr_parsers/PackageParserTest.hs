module PackageParserTest where

import Test.HUnit
import Text.Megaparsec

import Block
import ExprParser

testPackageParser :: Test
testPackageParser = do
    let code = "package dir.sub_dir"
    TestCase $ assertEqual code
        (Package ["dir","sub_dir"])
        (case (parse (packageParser) "" code) of
             Left  _ -> Error
             Right x -> x)
