module Test.Core (
  TestTree,
  Property,
  property,
  testGroup,
  testCase,
  testProp,
) where

import Hedgehog (Property, PropertyT, property, withTests)
import Test.Tasty (TestName, TestTree, testGroup)

import Test.Compat (testProp)

--------------------------------------------------------------------------------

testCase :: TestName -> PropertyT IO () -> TestTree
testCase name = testProp name . withTests 1 . property