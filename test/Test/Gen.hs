module Test.Gen (
  sized'list,
  length,
  nonzero'length,
  Gen.int,
  any'int,
  negative,
  positive,
) where

import Hedgehog (Gen, Range)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Prelude hiding (length)

--------------------------------------------------------------------------------

sized'list :: Gen a -> Gen [a]
sized'list gen = 
  Gen.sized \size -> 
    let bound :: Int 
        bound = max (fromIntegral size) 0 
     in Gen.list (Range.linear 0 bound) gen

-- | TODO: docs 
length :: Gen Int 
length = 
  Gen.sized \size -> 
    let bound :: Int 
        bound = max (fromIntegral size) 0
     in Gen.int (Range.linear 0 bound)

-- | TODO: docs 
nonzero'length :: Gen Int 
nonzero'length = 
  Gen.sized \size -> 
    let bound :: Int 
        bound = max (fromIntegral size) 1
     in Gen.int (Range.linear 1 bound)

-- | TODO: docs
any'int :: Gen Int 
any'int = Gen.int Range.constantBounded

-- | TODO: docs
negative :: (Bounded a, Integral a) => (Range a -> Gen a) -> Gen a
negative k = k (Range.linear minBound 0)

-- | TODO: docs
positive :: (Bounded a, Integral a) => (Range a -> Gen a) -> Gen a
positive k = k (Range.linear 0 maxBound)
