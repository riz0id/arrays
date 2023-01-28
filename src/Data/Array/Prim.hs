{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Array.Prim
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
module Data.Array.Prim
  ( Array#
    -- * Construction
  , empty#
  , singleton#
  , new#
    -- * Conversion
    -- * Memory
  , clone#
  , cloneToMutableArray#
  , MutArray.Prim.freeze#
  , MutArray.Prim.unsafeFreeze#
  , MutArray.Prim.thaw#
  , MutArray.Prim.unsafeThaw#
    -- * Query
  , length#
  , null#
    -- * Read
  , index#
  , read#
    -- * Write
  , write#
  ) where

import Control.Exception (toException)
import Control.Exception.RangeError (RangeError (..))

import Data.Bool.Prim (Bool# (..))
import Data.Bool.Prim qualified as Bool
import Data.MutArray.Prim qualified as MutArray.Prim

import GHC.Exts
  (Array#, Int (..), Int#, MutableArray#, RuntimeRep, State#, TYPE)
import GHC.Exts qualified as GHC

import Language.Haskell.TH.Syntax (Name)

--------------------------------------------------------------------------------

raiseRangeError# ::
  forall (r :: RuntimeRep) (e :: TYPE r).
  -- | The 'Name' of the function raising the 'RangeError'.
  Name ->
  -- | The offending index.
  Int# ->
  -- | The length of the 'MutableArray'
  Int# ->
  -- | TODO: docs
  e
raiseRangeError# fn i# len# =
  let exn :: RangeError
      exn = RangeError fn ''MutableArray# Nothing (I# i#) 0 (I# (len# GHC.-# 1#))
   in GHC.raise# (toException exn)

-- Construction ----------------------------------------------------------------

-- | Constructs a new, empty 'Array#'.
--
-- @since 1.0.0
empty# :: State# s -> (# State# s, Array# a #)
empty# st0# = case MutArray.Prim.empty# st0# of
  (# st1#, xs# #) -> GHC.unsafeFreezeArray# xs# st1#

-- | Constructs a singleton 'Array#' from the value provided.
--
-- @since 1.0.0
singleton# :: a -> State# s -> (# State# s, Array# a #)
singleton# x st0# = case GHC.newArray# 1# x st0# of
  (# st1#, mut# #) -> GHC.unsafeFreezeArray# mut# st1#

-- | TODO: docs
--
-- @since 1.0.0
new# :: Int# -> a -> State# s -> (# State# s, Array# a #)
new# n# x st0# = case 0# GHC.<=# n# of
  1# -> case GHC.newArray# n# x st0# of
    (# st1#, mut# #) -> GHC.unsafeFreezeArray# mut# st1#
  _  -> errorWithoutStackTrace (show 'new# ++ ": invalid length (argument #1): " ++ show (I# n#)) -- FIXME: canonicalize error

-- Conversion ------------------------------------------------------------------

-- Memory ----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). TODO: docs
--
-- @since 1.0.0
clone# ::
  -- | TODO: docs
  Array# a ->
  -- | TODO: docs
  Array# a
clone# src# = GHC.cloneArray# src# 0# (length# src#)

-- | \(\mathcal{O}(1)\). TODO: docs
--
-- @since 1.0.0
cloneToMutableArray# ::
  -- | TODO: docs
  Array# a ->
  -- | TODO: docs
  State# s ->
  -- | TODO: docs
  (# State# s, MutableArray# s a #)
cloneToMutableArray# src# st# =
  let !dst# = GHC.cloneArray# src# 0# (length# src#)
   in GHC.unsafeThawArray# dst# st#

-- Query -----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Obtain the length of an 'Array#'.
--
-- @since 1.0.0
length# :: Array# a -> Int#
length# = GHC.sizeofArray#

-- | \(\mathcal{O}(1)\). Is the given 'Array#' empty?
--
-- @since 1.0.0
null# :: Array# a -> Bool#
null# xs# = Bool.unsafeFromInt# (length# xs# GHC.==# 0#)

-- Read ------------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). TODO: docs
--
-- @since 1.0.0
index# ::
  -- | TODO: docs
  Array# a ->
  -- | TODO: docs
  Int# ->
  -- | TODO: docs
  (# (# #) | a #)
index# xs# i# =
  let !len# = GHC.sizeofArray# xs#
   in case GHC.andI# (0# GHC.<=# i#) (i# GHC.<# len#) of
        1# -> case GHC.indexArray# xs# i# of (# x #) -> (# | x #)
        _  -> (# (# #) | #)

-- | \(\mathcal{O}(1)\). Reads a value from a source 'MutableArray#' at the
-- given index. The given index should be greater than or equal to @0#@ and
-- strictly less than the length of the source array. Otherwise, 'read#' will
-- throw an unchecked 'IndexException'.
--
-- @since 1.0.0
read# ::
  -- | The source array to read from.
  Array# a ->
  -- | TODO: docs
  Int# ->
  -- | TODO: docs
  a
read# xs# i# =
  let !len# = GHC.sizeofArray# xs#
   in case GHC.andI# (0# GHC.<=# i#) (i# GHC.<# len#) of
        1# -> case GHC.indexArray# xs# i# of (# x #) -> x
        _  -> raiseRangeError# 'read# i# len# 

-- Write -----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). The function @('write' xs i x :: m ())@ writes the
-- element @(x :: a)@ to position @(i :: 'Int')@ in the 'MutableArray' @xs@. If
-- the given index @(i :: 'Int')@ is outside of the bounds (i.e. the less-than
-- zero or strictly greater-than the length of @(xs :: 'MutableArray'
-- ('PrimState' m) a)@, then a 'Control.Exception.RangeError' will be thrown.
--
-- @since 1.0.0
write# ::
  -- | The target 'Array' to write an element to.
  Array# a ->
  -- | The position in the 'Array' to write the given element to. Must be
  -- greater-than or equal-to @(0 :: 'Int')@, and less-than the length of the
  -- given 'Array'.
  Int# ->
  -- | The element to write.
  a ->
  -- | TODO: docs
  Array# a
write# src# i# x =
  let !len# = GHC.sizeofArray# src#
   in case GHC.andI# (0# GHC.<=# i#) (i# GHC.<# len#) of
        1# -> GHC.runRW# \st0# ->
          let !(# st1#, mut# #) = cloneToMutableArray# src# st0#
              !st2# = GHC.writeArray# mut# i# x st1#
              !(# _,    dst# #) = GHC.unsafeFreezeArray# mut# st2#
           in dst#
        _  -> raiseRangeError# 'read# i# len# 


