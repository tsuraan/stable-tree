-- |
-- Module    : Data.StableTree
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- A B-Tree variation designed for maximal stability under mutation. The
-- StableTree structure is meant to be used in places where different versions
-- of a key/value map are kept, such as in a versioning file system or a
-- revision control system. As a tree's contents are mutated (inserted,
-- updated, deleted), it will tend to keep the vast majority of its branches
-- untouched, with generally just the lowest branch and its immediate ancestor
-- chain being modified. Put another way, trees with similar contents will also
-- share a majority of their internal branches.
--
-- This module exports the public interface for StableTree. It largely mimics
-- the Data.Map interface, so it should be fairly familiar to Haskell users.
module Data.StableTree
( StableTree
, fromMap
, empty
, insert
, delete
, size
, lookup
, keys
, elems
, assocs
, append
, concat
, toMap
) where

import Data.StableTree.Build      ( fromMap, empty, append, concat )
import Data.StableTree.Mutate     ( insert, delete )
import Data.StableTree.Properties ( toMap, size, lookup, keys, elems, assocs )
import Data.StableTree.Types      ( StableTree )

import Prelude ()

