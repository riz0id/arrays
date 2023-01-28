{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.MutArray.Prim
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
module Data.MutArray.Prim
  ( MutableArray#
    -- * Construction
  , empty#
  , singleton#
  , new#
    -- * Conversion
    -- * Copy
  , clone#
    -- * Memory
  , freeze#
  , unsafeFreeze#
  , thaw#
  , unsafeThaw#
    -- * Query
  , length#
  , null#
  , isIndexInBounds#
    -- * Index
  , index#
  , read#
    -- * Write
  , write#
  ) where

import Control.Exception (toException)
import Control.Exception.ElementError (ElementError (..))
import Control.Exception.RangeError (RangeError (..))

import Data.Bool.Prim (Bool# (..))
import Data.Bool.Prim qualified as Bool

import GHC.Exts
  (Array#, Int (..), Int#, MutableArray#, RuntimeRep, State#, TYPE)
import GHC.Exts qualified as GHC

import Language.Haskell.TH (Name)

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

--------------------------------------------------------------------------------

-- | Constructs a new, empty 'MutableArray#'.
--
-- @since 1.0.0
empty# :: State# s -> (# State# s, MutableArray# s a #)
empty# =
  let exn :: ElementError
      exn = ElementErrorAtLoc ''MutableArray# 0
   in GHC.newArray# 0# (GHC.raise# (toException exn))

-- | Constructs a singleton 'MutableArray#' from the value provided.
--
-- @since 1.0.0
singleton# :: a -> State# s -> (# State# s, MutableArray# s a #)
singleton# = GHC.newArray# 1#

-- | TODO: docs
--
-- @since 1.0.0
new# :: Int# -> a -> State# s -> (# State# s, MutableArray# s a #)
new# n# x st# = case 0# GHC.<=# n# of
  1# -> GHC.newArray# n# x st#
  _  -> errorWithoutStackTrace (show 'new# ++ ": invalid length (argument #1): " ++ show (I# n#)) -- FIXME: canonicalize error

-- Conversion ------------------------------------------------------------------

-- Copy ------------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Allocates a new 'MutableArray#' with the same elements
-- as the given 'MutableArray#'.
--
-- @since 1.0.0
clone# ::
  -- | The target 'MutableArray#' to clone.
  MutableArray# s a ->
  -- | The 'State#' token.
  State# s ->
  -- | Returns a new copy of the given 'MutableArray#', along with a fresh
  -- 'State#' token.
  (# State# s, MutableArray# s a #)
clone# xs# = GHC.cloneMutableArray# xs# 0# (length# xs#)

-- Memory ----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Copies the target 'MutableArray#' to produce in a new,
-- identical, and immutable 'Array#'.
--
-- @since 1.0.0
freeze# :: MutableArray# s a -> State# s -> (# State# s, Array# a #)
freeze# src# st0# = case clone# src# st0# of
  (# st1#, dst# #) -> GHC.unsafeFreezeArray# dst# st1#

-- | \(\mathcal{O}(1)\). Freezes the given 'MutableArray' to produce an
-- immutable 'Array'. Unlike 'freeze', 'unsafeFreeze' does not copy the given
-- 'MutableArray'.
--
-- @since 1.0.0
unsafeFreeze# :: MutableArray# s a -> State# s -> (# State# s, Array# a #)
unsafeFreeze# = GHC.unsafeFreezeArray#

-- | TODO: docs
--
-- @since 1.0.0
thaw# :: Array# a -> State# s -> (# State# s, MutableArray# s a #)
thaw# src# st0# =
  case GHC.unsafeThawArray# src# st0# of
    (# st1#, dst# #) -> clone# dst# st1#

-- | TODO: docs
--
-- @since 1.0.0
unsafeThaw# :: Array# a -> State# s -> (# State# s, MutableArray# s a #)
unsafeThaw# = GHC.unsafeThawArray#

-- Query -----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Obtain the length of a 'MutableArray#'.
--
-- @since 1.0.0
length# :: MutableArray# s a -> Int#
length# = GHC.sizeofMutableArray#

-- | \(\mathcal{O}(1)\). Is the given 'MutableArray#' empty?
--
-- @since 1.0.0
null# :: MutableArray# s a -> Bool#
null# xs# = Bool.unsafeFromInt# (length# xs# GHC.==# 0#)

-- | TODO: docs
--
-- @since 1.0.0
isIndexInBounds# :: MutableArray# s a -> Int# -> Bool#
isIndexInBounds# xs# i# =
  let !lowerB# = Bool.unsafeFromInt# (0# GHC.<=# i#)
      !upperB# = Bool.unsafeFromInt# (i# GHC.<# length# xs#)
   in Bool.and# lowerB# upperB#

-- Index -----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). TODO: docs
--
-- @since 1.0.0
index# ::
  -- | TODO: docs
  MutableArray# s a ->
  -- | TODO: docs
  Int# ->
  -- | TODO: docs
  State# s ->
  -- | TODO: docs
  (# State# s, (# (# #) | a #) #)
index# xs# n# st0# =
  case isIndexInBounds# xs# n# of
    T# -> case GHC.readArray# xs# n# st0# of
      (# st1#, x #) -> (# st1#, (# | x #) #)
    F# -> (# st0#, (# (# #) | #) #)

-- | \(\mathcal{O}(1)\). Reads a value from a source 'MutableArray#' at the
-- given index. The given index should be greater than or equal to @0#@ and
-- strictly less than the length of the source array. Otherwise, 'read#' will
-- throw an unchecked 'IndexException'.
--
-- @since 1.0.0
read# ::
  -- | The source array to read from.
  MutableArray# s a ->
  -- | TODO: docs
  Int# ->
  -- | The state token to use when accessing the source array.
  State# s ->
  -- | TODO: docs
  (# State# s, a #)
read# xs# i# st# =
  case isIndexInBounds# xs# i# of
    T# -> GHC.readArray# xs# i# st#
    F# -> raiseRangeError# 'read# i# (GHC.sizeofMutableArray# xs#)

-- Write -----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). TODO: docs
--
-- @since 1.0.0
write# ::
  -- | TODO: docs
  MutableArray# s a ->
  -- | TODO: docs
  Int# ->
  -- | TODO: docs
  a ->
  -- | TODO: docs
  State# s ->
  -- | TODO: docs
  State# s
write# xs# i# x st0# =
  case isIndexInBounds# xs# i# of
    T# -> GHC.writeArray# xs# i# x st0#
    F# -> raiseRangeError# 'write# i# (GHC.sizeofMutableArray# xs#)
