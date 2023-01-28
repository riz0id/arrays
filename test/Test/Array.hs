{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Array (testTree) where

import Control.Exception (try, displayException, evaluate, fromException, ErrorCall (..))

import Data.Array qualified as Array
import Data.Foldable (toList)

import GHC.Exts (fromList)

import Hedgehog (evalIO, failure, footnote, forAll, (===))
import Hedgehog.Gen qualified as Gen

import Test.Core (Property, TestTree, property, testCase, testGroup, testProp)
import Test.Gen qualified as Gen
import qualified Data.Array.Prim as Array.Prim

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "array"
    [ testCase "empty" do 
        int <- forAll Gen.any'int
        Array.new 0 int === Array.empty
    , testGroup
        "new"
        [ testCase "empty" do 
            int <- forAll Gen.any'int
            toList (Array.new 0 int) === []
        , testCase "singleton" do
            int <- forAll Gen.any'int
            toList (Array.singleton int) === [int]
        , testProp "many" $ property do 
            len <- forAll Gen.length 
            int <- forAll Gen.any'int
            toList (Array.new len int) === replicate len int
        ]
    , testGroup 
        "length" 
        [ testCase "empty" do 
            int <- forAll Gen.any'int
            Array.length (Array.new 0 int) === 0
        , testCase "singleton" do
            int <- forAll Gen.any'int
            Array.length (Array.singleton int) === 1
        , testProp "many" $ property do 
            len <- forAll Gen.length 
            int <- forAll Gen.any'int
            Array.length (Array.new len int) === len
        ]
    , testGroup 
        "null" 
        [ testCase "empty" do 
            Array.null Array.empty === True
        , testProp "nonempty" $ property do 
            len <- forAll Gen.nonzero'length 
            int <- forAll Gen.any'int
            Array.null (Array.new len int) === False
        ]
    , testProp "list" $ property do 
        items <- forAll (Gen.sized'list Gen.any'int)
        array <- evalIO (evaluate (fromList items))
        items === fromList array
    , testGroup
        "fail"
        [ testProp "new" propFailAllocError
        ]
    ]

-- | TODO: docs
propFailAllocError :: Property
propFailAllocError = property do
  len <- forAll (Gen.negative Gen.int)
  evalIO (try (evaluate (Array.new len ()))) >>= \case
    Left ret -> case fromException @ErrorCall ret of
      Nothing -> do
        footnote (displayException ret)
        failure
      Just exn -> do
        let desc = show 'Array.Prim.new# ++ ": invalid length (argument #1): " ++ show len 
        exn === ErrorCall desc 
    Right ret -> do
      footnote "expected: failure due to allocation exception"
      footnote ("actual: successful evaluation resulting in: " ++ show ret)
      failure
