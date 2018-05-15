module TestUtil.ParserTestUtil where

import Test.HUnit
import Text.Megaparsec

testParseSuccess code result parser = TestCase $ assertEqual code
    result
    (case (parse (parser <* eof ) "" code) of
        Left  e -> error $ "(" ++ code ++ ") - " ++ show e
        Right x -> x)

testParseFailure code parser = TestCase $ assertEqual code
    TMPError
    (case (parse parser "" code) of
        Left  _ -> TMPError
        Right x -> TMPValue x)

data TMPHelper a = TMPError
                | TMPValue a
                deriving (Show, Eq)
