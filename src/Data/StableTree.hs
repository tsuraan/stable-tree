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
( StableTree(..)
, fromMap
, toMap
, Error(..)
, load
, load'
, store
, store'
) where

import qualified Data.StableTree.Tree as Tree
import Data.StableTree.Persist ( Error(..), load, load', store, store' )
import Data.StableTree.Tree    ( StableTree(..) )

import qualified Data.Map as Map
import Data.Map       ( Map )
import Data.Maybe     ( isNothing )
import Data.Serialize ( Serialize )

-- | Convert a 'Data.Map.Map' into a 'StableTree'.
fromMap :: (Ord k, Serialize k, Serialize v) => Map k v -> StableTree k v
fromMap m = go m Map.empty
  where
  go values accum =
    case Tree.nextBottom values of
      Left incomplete ->
        if Map.null accum
          then StableTree_I incomplete
          else case Tree.getKey incomplete of
            Just k  -> buildParents accum (Just (k, incomplete)) Map.empty
            Nothing -> buildParents accum Nothing Map.empty
      Right (complete, remain) ->
        if Map.null remain && Map.null accum
          then StableTree_C complete
          else go remain $ Map.insert (Tree.completeKey complete) complete accum

  buildParents completes mIncomplete accum =
    case Tree.nextBranch completes mIncomplete of
      Left incomplete ->
        if Map.null accum
          then StableTree_I incomplete
          else case Tree.getKey incomplete of
            Just k  -> buildParents accum (Just (k, incomplete)) Map.empty
            Nothing -> buildParents accum Nothing Map.empty
      Right (complete, remain) ->
        if Map.null remain && Map.null accum && isNothing mIncomplete
          then StableTree_C complete
          else 
            let accum' = Map.insert (Tree.completeKey complete) complete accum
            in buildParents remain mIncomplete accum'

-- | Convert a 'StableTree' back into a 'Data.Map.Map'
toMap :: Ord k => StableTree k v -> Map k v
toMap (StableTree_I t) = Tree.treeContents t
toMap (StableTree_C t) = Tree.treeContents t

