-- |
-- Module    : Data.StableTree
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- A Rose Tree designed for maximal stability under mutation. The StableTree
-- structure is meant to be used in places where different versions of a
-- key/value map are kept, such as in a versioning file system or a revision
-- control system. As a tree's contents are mutated (inserted, updated,
-- deleted), it will tend to keep the vast majority of its branches untouched,
-- with generally just the immediate branch and its immediate ancestor chain
-- being modified. Put another way, trees with similar contents will also share
-- a majority of their branches.
--
-- This module exports the public interface for StableTree. Right now, that's
-- just a translation to the standard Data.Map and back. There's nothing about
-- StableTree that forbids direct manipulation, but I've been playing with
-- various implementations of this for way too long, and I just want to start
-- using the dang thing now.
module Data.StableTree
( StableTree
, fromMap
, empty
, insert
, delete
, size
, lookup
, keys
, fmap
, append
, concat
, toMap
) where

import Data.StableTree.Build      ( fromMap, empty, append, concat )
import Data.StableTree.Mutate     ( insert, delete, fmap )
import Data.StableTree.Properties ( toMap, size, lookup, keys )
import Data.StableTree.Types      ( StableTree )

import Prelude ()

