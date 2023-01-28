
module Main (main) where

import Test.Array qualified
import Test.Compat (TestTree, testGroup)
import Test.Tasty (defaultMain)

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup
    "test"
    [ Test.Array.testTree
    ]