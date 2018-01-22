import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Utils

pushTest :: Assertion
pushTest = [NumLit 1] ^? push (NumLit 1)

pushPopTest :: Assertion
pushPopTest = [] ^? (push (NumLit 0) >> void pop)

main :: IO ()
main = defaultMainWithOpts
       [testCase "push" pushTest
       ,testCase "push-pop" pushPopTest]
       mempty