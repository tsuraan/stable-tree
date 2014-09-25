-- |
-- Module    : Data.StableTree.Types
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- Definitions of primitive types used in different modules of stable-tree
module Data.StableTree.Types
( Depth
, ValueCount
) where

-- |Alias to indicate how deep a branch in a tree is. Bottoms have depth 0
type Depth = Int

-- |Alias that indicates the total number of values underneath a tree
type ValueCount = Int

