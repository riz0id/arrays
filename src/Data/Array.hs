{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.MutArray
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Data.Array
  ( Array
    -- * Construction
  , empty
  , singleton
  , new
    -- * Memory
  , clone
  , cloneToMutableArray
    -- * Compare
    -- * Query
  , length
  , null
    -- * Read
  , index
  , read
    -- * Write
  , write
  ) where

import Control.Monad.Primitive (PrimMonad (..))

import Data.Array.Prim qualified as Prim
import Data.Bool.Prim qualified as Bool
import Data.Primitive.Array (Array (..), MutableArray (..))

import GHC.Exts (Int (..))

import Prelude hiding (length, null, read)
import qualified GHC.Exts as GHC

-- Construction ----------------------------------------------------------------

-- | Allocate a new, empty 'Array'.
--
-- @since 1.0.0
empty :: Array a
empty =
  GHC.runRW# \st0# -> 
    case Prim.empty# st0# of
      (# _, xs# #) -> Array xs# 
{-# INLINE empty #-}

-- | Allocate a singleton 'Array' from the value provided.
--
-- @since 1.0.0
singleton ::
  -- | The value populating the 'Array' singleton.
  a ->
  -- | Returns a new 'Array' of length @(1 :: 'Int')@.
  Array a
singleton x =
  GHC.runRW# \st0# -> 
    case Prim.singleton# x st0# of
      (# _, xs# #) -> Array xs# 
{-# INLINE singleton #-}

-- | Allocate a new 'Array' of a given length. All elements in the array will be
-- set to the given initial value.
--
-- @since 1.0.0
new ::
  -- | The requested length of the 'Array'. Must be greater-than or equal-to
  -- @(0 :: Int)@.
  Int ->
  -- | The initial value for all elements in the new 'Array'.
  a ->
  -- | Returns a new 'Array' of the given length.
  Array a
new (I# n#) x =
  GHC.runRW# \st0# -> 
    case Prim.new# n# x st0# of
      (# _, xs# #) -> Array xs# 
{-# INLINE new #-}

-- Conversion ------------------------------------------------------------------

-- Memory ----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Allocates a new 'Array' with the same elements as the
-- given 'Array'. 
--
-- @since 1.0.0
clone ::
  -- | The target 'Array' to clone.
  Array a ->
  -- | Returns a new copy of the given 'Array'.
  Array a
clone (Array src#) = Array (Prim.clone# src#)
{-# INLINE clone #-}

-- | \(\mathcal{O}(1)\). Allocates a new 'MutableArray' with the same elements
-- as the given 'Array'. 
--
-- @since 1.0.0
cloneToMutableArray ::
  PrimMonad m =>
  -- | The target 'Array' to clone.
  Array a ->
  -- | Returns a new 'MutableArray' copy of the given 'Array'.
  m (MutableArray (PrimState m) a)
cloneToMutableArray (Array src#) =
  primitive \st0# -> case Prim.cloneToMutableArray# src# st0# of
    (# st1#, dst# #) -> (# st1#, MutableArray dst# #)
{-# INLINE cloneToMutableArray #-}

-- Compare ---------------------------------------------------------------------

-- Query -----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Obtain the length of a 'Array'.
--
-- @since 1.0.0
length :: Array a -> Int
length (Array xs#) = I# (Prim.length# xs#)
{-# INLINE length #-}

-- | \(\mathcal{O}(1)\). Is the given 'Array' empty?
--
-- @since 1.0.0
null :: Array a -> Bool
null (Array xs#) = Bool.toBool (Prim.null# xs#)
{-# INLINE null #-}

-- Read ------------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Similar to 'read', the function @('index' xs i :: a)@
-- reads the element at the position @(i :: 'Int')@ in the 'Array' @xs@. If the
-- given index @(i :: 'Int')@ is out of the bounds (i.e. the less-than zero or
-- strictly greater-than the length of @xs@), then the result will be
-- @('Nothing' :: a)@.
--
-- @since 1.0.0
index ::
  -- | The target 'Array' being indexed in to.
  Array a ->
  -- | The position to index the 'Array' at.
  Int ->
  -- | If the given index is within the bounds of the 'Array' being indexed,
  -- then the element at that index is returned. Otherwise, the result is
  -- 'Nothing'.
  Maybe a
index (Array xs#) (I# i#) =
  case Prim.index# xs# i# of
    (# (# #) | #) -> Nothing
    (#   |   x #) -> Just x
{-# INLINE index #-}

-- | \(\mathcal{O}(1)\). The function @('index' xs i :: a)@ reads the element
-- at the position @(i :: 'Int')@ in the 'Array' @xs@. If the given index
-- @(i :: 'Int')@ is outside of the bounds (i.e. the less-than zero or strictly
-- greater-than the length of @(xs :: 'Array' a)@, then a
-- 'Control.Exception.RangeError' will be thrown.
--
-- __NOTE__: It is only wise to opt for 'read' over 'index' when accessing a
-- 'Array' when it is guaranteed the given index @(i :: 'Int')@ will never fall
-- outside the bounds of the target 'Array'. In all other cases, using
-- the much safer 'index' function for 'Array' access is recommended.
--
-- @since 1.0.0
read ::
  -- | The target 'Array' being indexed in to.
  Array a ->
  -- | The position to index the 'Array' at. Must be greater-than or
  -- equal-to @(0 :: 'Int')@, and less-than the length of the given
  -- 'Array'.
  Int ->
  -- | The element at the given index of the 'Array'.
  a
read (Array xs#) (I# i#) = Prim.read# xs# i#
{-# INLINE read #-}

-- Write -----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). The function @('write' xs i x :: m ())@ copies the
-- 'Array' @xs@, and writes the element @(x :: a)@ to position @(i :: 'Int')@ in
-- the new copy. If the given index @(i :: 'Int')@ is outside of the bounds
-- (i.e. the less-than zero or strictly greater-than the length of @(xs ::
-- 'Array' ('PrimState' m) a)@, then a 'Control.Exception.RangeError' will be
-- thrown.
--
-- @since 1.0.0
write ::
  -- | The target 'Array' to write an element to.
  Array a ->
  -- | The position in the 'Array' to write the given element to. Must be
  -- greater-than or equal-to @(0 :: 'Int')@, and less-than the length of the
  -- given 'Array'.
  Int ->
  -- | The element to write.
  a ->
  -- | Returns a new 'Array', with the given element written.
  Array a
write (Array src#) (I# i#) x = Array (Prim.write# src# i# x)
{-# INLINE write #-}
