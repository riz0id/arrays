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
module Data.MutArray
  ( MutableArray
    -- * Construction
  , empty
  , singleton
  , new
    -- * Memory
  , clone
  , freeze
  , unsafeFreeze
  , thaw
  , unsafeThaw
    -- * Compare
    -- * Query
  , length
  , null
  , isIndexInBounds
    -- * Index
  , index
  , read
    -- * Write 
  , write
  ) where

import Control.Monad.Primitive (PrimMonad (..))

import Data.Bool.Prim qualified as Bool
import Data.MutArray.Prim qualified as Prim
import Data.Primitive.Array (Array (..), MutableArray (..))

import GHC.Exts (Int (..))

import Prelude hiding (length, null, read)

-- Construction ----------------------------------------------------------------

-- | Allocate a new, empty 'MutableArray'.
--
-- @since 1.0.0
empty :: PrimMonad m => m (MutableArray (PrimState m) a)
empty =
  primitive \st0# -> case Prim.empty# st0# of
    (# st1#, xs# #) -> (# st1#, MutableArray xs# #)
{-# INLINE empty #-}

-- | Allocate a singleton 'MutableArray' from the value provided.
--
-- @since 1.0.0
singleton :: 
  PrimMonad m => 
  -- | The value populating the 'MutableArray' singleton.
  a -> 
  -- | Returns a new 'MutableArray' of length @(1 :: 'Int')@.
  m (MutableArray (PrimState m) a)
singleton x =
  primitive \st0# -> case Prim.singleton# x st0# of
    (# st1#, xs# #) -> (# st1#, MutableArray xs# #)
{-# INLINE singleton #-}

-- | Allocate a new 'MutableArray' of a given length. All elements in the array 
-- will be set to the given initial value.
--
-- @since 1.0.0
new :: 
  PrimMonad m => 
  -- | The requested length of the 'MutableArray'. Must be greater-than or 
  -- equal-to @(0 :: Int)@.
  Int -> 
  -- | The initial value for all elements in the new 'MutableArray'. 
  a -> 
  -- | Returns a new 'MutableArray' of the given length. 
  m (MutableArray (PrimState m) a)
new (I# n#) x =
  primitive \st0# -> case Prim.new# n# x st0# of
    (# st1#, xs# #) -> (# st1#, MutableArray xs# #)
{-# INLINE new #-}

-- Conversion ------------------------------------------------------------------

-- Memory ----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Allocates a new 'MutableArray' with the same elements 
-- as the given 'MutableArray'. 
--
-- @since 1.0.0
clone :: 
  PrimMonad m => 
  -- | The target 'MutableArray' to clone.
  MutableArray (PrimState m) a -> 
  -- | Returns a new copy of the given 'Array'.
  m (MutableArray (PrimState m) a)
clone (MutableArray xs0#) =
  primitive \st0# -> case Prim.clone# xs0# st0# of
    (# st1#, xs1# #) -> (# st1#, MutableArray xs1# #)
{-# INLINE clone #-}

-- | \(\mathcal{O}(1)\). Copies the target 'MutableArray' to produce in a new,
-- identical, and immutable 'Array'. 
--
-- @since 1.0.0
freeze :: 
  PrimMonad m => 
  -- | The target 'MutableArray' to freeze.
  MutableArray (PrimState m) a -> 
  -- | Returns an immutable copy of the given 'MutableArray'.
  m (Array a)
freeze (MutableArray xs0#) =
  primitive \st0# -> case Prim.freeze# xs0# st0# of
    (# st1#, xs1# #) -> (# st1#, Array xs1# #)
{-# INLINE freeze #-}

-- | \(\mathcal{O}(1)\). Freezes the given 'MutableArray' to produce an 
-- immutable 'Array'. Unlike 'freeze', 'unsafeFreeze' does not copy the given 
-- 'MutableArray'.
--
-- @since 1.0.0
unsafeFreeze ::
  PrimMonad m => 
  -- | The target 'MutableArray' to freeze.
  MutableArray (PrimState m) a -> 
  -- | Returns an immutable version of the given 'MutableArray'.
  m (Array a)
unsafeFreeze (MutableArray xs0#) =
  primitive \st0# -> case Prim.unsafeFreeze# xs0# st0# of
    (# st1#, xs1# #) -> (# st1#, Array xs1# #)
{-# INLINE unsafeFreeze #-}

-- | \(\mathcal{O}(1)\). Copies the contents of the target 'Array' in to a newly 
-- allocated 'MutableArray'.
--
-- @since 1.0.0
thaw ::  
  PrimMonad m => 
  -- | The target 'MutableArray' to thaw.
  Array a ->
  -- | Returns an mutable version of the given 'Array'.
  m (MutableArray (PrimState m) a)
thaw (Array xs0#) = 
  primitive \st0# -> case Prim.thaw# xs0# st0# of
    (# st1#, xs1# #) -> (# st1#, MutableArray xs1# #)
{-# INLINE thaw #-}

-- | \(\mathcal{O}(1)\). Thaws the given 'Array', returning the same array as 
-- 'MutableArray'. Unlike 'thaw', 'unsafeThaw' does not copy the given 'Array'.
--
-- @since 1.0.0
unsafeThaw ::
  PrimMonad m => 
  -- | The target 'MutableArray' to thaw.
  Array a -> 
  -- | Returns an mutable version of the given 'Array'.
  m (MutableArray (PrimState m) a)
unsafeThaw (Array xs0#) =
  primitive \st0# -> case Prim.unsafeThaw# xs0# st0# of
    (# st1#, xs1# #) -> (# st1#, MutableArray xs1# #)
{-# INLINE unsafeThaw #-}

-- Compare ---------------------------------------------------------------------


-- Query -----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Obtain the length of a 'MutableArray'.
--
-- @since 1.0.0
length :: MutableArray s a -> Int
length (MutableArray xs#) = I# (Prim.length# xs#)
{-# INLINE length #-}

-- | \(\mathcal{O}(1)\). Is the given 'MutableArray' empty?
--
-- @since 1.0.0
null :: MutableArray s a -> Bool
null (MutableArray xs#) = Bool.toBool (Prim.null# xs#)
{-# INLINE null #-}

-- | TODO: docs
--
-- @since 1.0.0
isIndexInBounds :: MutableArray s a -> Int -> Bool
isIndexInBounds (MutableArray xs#) (I# i#) = Bool.toBool (Prim.isIndexInBounds# xs# i#)
{-# INLINE isIndexInBounds #-}

-- Index -----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Similar to 'read', the function @('index' xs i :: m a)@ 
-- reads the element at the position @(i :: 'Int')@ in the 'MutableArray' @xs@. 
-- If the given index @(i :: 'Int')@ is out of the bounds (i.e. the less-than 
-- zero or strictly greater-than the length of @xs@), then the result will be 
-- @('pure' 'Nothing' :: m a)@.
--
-- @since 1.0.0
index :: 
  PrimMonad m => 
  -- | The target 'MutableArray' being indexed in to.
  MutableArray (PrimState m) a -> 
  -- | The position to index the 'MutableArray' at. 
  Int -> 
  -- | If the given index is within the bounds of the 'MutableArray' being 
  -- indexed, then the element at that index is returned. Otherwise, the result 
  -- is 'Nothing'.
  m (Maybe a)
index (MutableArray xs#) (I# i#) =
  primitive \st0# -> case Prim.index# xs# i# st0# of
    (# st1#, (# | x #) #)     -> (# st1#, Just x #)
    (# st1#, (# (# #) | #) #) -> (# st1#, Nothing #)
{-# INLINE index #-}

-- | \(\mathcal{O}(1)\). The function @('index' xs i :: m a)@ reads the element 
-- at the position @(i :: 'Int')@ in the 'MutableArray' @xs@. If the given index 
-- @(i :: 'Int')@ is outside of the bounds (i.e. the less-than zero or strictly
-- greater-than the length of @(xs :: 'MutableArray' ('PrimState' m) a)@, then a
-- 'Control.Exception.RangeError' will be thrown. 
--
-- __NOTE__: It is only wise to opt for 'read' over 'index' when accessing a 
-- 'MutableArray' when it is guaranteed the given index @i@ will never fall 
-- outside the bounds of the target 'MutableArray'. In all other cases, using 
-- the much safer 'index' function for 'MutableArray' access is recommended.
--
-- @since 1.0.0
read :: 
  PrimMonad m => 
  -- | The target 'MutableArray' being indexed in to.
  MutableArray (PrimState m) a -> 
  -- | The position to index the 'MutableArray' at. Must be greater-than or 
  -- equal-to @(0 :: 'Int')@, and less-than the length of the given 
  -- 'MutableArray'.
  Int -> 
  -- | The element at the given index of the 'MutableArray'.
  m a
read (MutableArray xs#) (I# i#) = primitive (Prim.read# xs# i#)
{-# INLINE read #-}

-- Write -----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). The function @('write' xs i x :: m ())@ writes the 
-- element @(x :: a)@ to position @(i :: 'Int')@ in the 'MutableArray' @xs@. If 
-- the given index @(i :: 'Int')@ is outside of the bounds (i.e. the less-than 
-- zero or strictly greater-than the length of @(xs :: 'MutableArray' 
-- ('PrimState' m) a)@, then a 'Control.Exception.RangeError' will be thrown. 
--
-- @since 1.0.0
write ::
  PrimMonad m =>
  -- | The target 'MutableArray' being written to.
  MutableArray (PrimState m) a ->
  -- | The position in the 'MutableArray' to write the given element to. Must be
  -- greater-than or equal-to @(0 :: 'Int')@, and less-than the length of the 
  -- given 'MutableArray'.
  Int ->
  -- | The element to write.
  a ->
  m ()
write (MutableArray xs#) (I# i#) x =
  primitive \st0# -> (# Prim.write# xs# i# x st0#, () #)
{-# INLINE write #-}
