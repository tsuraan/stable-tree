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
, IsKey(..)
, fromMap
, toMap
) where

import Data.StableTree.Types

import qualified Data.Map as Map
import Data.Map ( Map )
import Data.Maybe ( isNothing )

-- | @StableTree@ is the opaque type that wraps the actual 'Tree'
-- implementation. All the public functions operate on this type.
data StableTree k v = StableTree_I (Tree Incomplete k v)
                    | StableTree_C (Tree Complete k v)

-- | Convert a 'Data.Map.Map' into a 'StableTree'.
fromMap :: (Ord k, IsKey k) => Map k v -> StableTree k v
fromMap m = go m Map.empty
  where
  go values accum =
    case nextBottom values of
      Left incomplete ->
        if Map.null accum
          then StableTree_I incomplete
          else case getKey incomplete of
            Just k  -> buildParents accum (Just (k, incomplete)) Map.empty
            Nothing -> buildParents accum Nothing Map.empty
      Right (complete, remain) ->
        if Map.null remain && Map.null accum
          then StableTree_C complete
          else go remain $ Map.insert (completeKey complete) complete accum

  buildParents completes mIncomplete accum =
    case nextBranch completes mIncomplete of
      Left incomplete ->
        if Map.null accum
          then StableTree_I incomplete
          else case getKey incomplete of
            Just k  -> buildParents accum (Just (k, incomplete)) Map.empty
            Nothing -> buildParents accum Nothing Map.empty
      Right (complete, remain) ->
        if Map.null remain && Map.null accum && isNothing mIncomplete
          then StableTree_C complete
          else 
            let accum' = Map.insert (completeKey complete) complete accum
            in buildParents remain mIncomplete accum'

-- | Convert a 'StableTree' back into a 'Data.Map.Map'
toMap :: Ord k => StableTree k v -> Map k v
toMap (StableTree_I t) = treeContents t
toMap (StableTree_C t) = treeContents t

instance (Ord k, Show k, Show v) => Show (StableTree k v) where
  show (StableTree_I t) = show t
  show (StableTree_C t) = show t
