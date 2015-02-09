{-# LANGUAGE LambdaCase, GADTs #-}
-- |
-- Module    : Data.StableTree
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- Functions for "updating" StableTrees, in the functional sense. This covers
-- insertion, deletion, etc.
module Data.StableTree.Mutate
( insert
, delete
) where

import Data.StableTree.Build      ( consume, consumeBranches', consumeMap, merge )
import Data.StableTree.Key        ( StableKey )
import Data.StableTree.Properties ( bottomChildren, selectNode )
import Data.StableTree.Types

import qualified Data.Map as Map
import Data.Map     ( Map )

-- |Insert a key/value into a 'StableTree'. If the key exists, its existing
-- value is overwritten.
insert :: (Ord k, StableKey k)
       => k -> v -> StableTree k v -> StableTree k v
insert k v (StableTree_C c) = (uncurry consume) $ insert' k v c
insert k v (StableTree_I i) = (uncurry consume) $ insert' k v i

-- |Remove a key from the 'StableTree'. If the key is not found, the tree is
-- returned unchanged.
delete :: (Ord k, StableKey k)
       => k -> StableTree k v -> StableTree k v
delete k (StableTree_C c) = (uncurry consume) $ delete' k c
delete k (StableTree_I i) = (uncurry consume) $ delete' k i

-- |Same as 'insert', but works on a 'Tree', and returns a list of completes
-- and a maybe incomplete instead of returning something that probably can't be
-- expressed in Haskell's type system.
insert' :: (Ord k, StableKey k)
        => k
        -> v
        -> Tree d c k v
        -> ([Tree d Complete k v], Maybe (Tree d Incomplete k v))
insert' k v = mutateBottom k $ Map.insert k v

-- |Same as 'delete', but works on a 'Tree', and returns a list of completes
-- and a maybe incomplete instead of returning something that probably can't be
-- expressed in Haskell's type system.
delete' :: (Ord k, StableKey k)
        => k
        -> Tree d c k v
        -> ([Tree d Complete k v], Maybe (Tree d Incomplete k v))
delete' k = mutateBottom k $ Map.delete k

-- |Find the 'Tree Z' instance that should contain the given key, and call the
-- given function on its contents. Once that's done, splice the result back
-- into a new tree, which will probably be really similar to the original, but
-- have the desired changes applied.
mutateBottom :: (Ord k, StableKey k)
             => k
             -> (Map k v -> Map k v)
             -> Tree d c k v
             -> ([Tree d Complete k v], Maybe (Tree d Incomplete k v))
mutateBottom search_key mut_fn = \case
    bottom@(Bottom _ _ _ _)     -> consumeMap $ mut_fn $ bottomChildren bottom
    bottom@(IBottom0 _)         -> consumeMap $ mut_fn $ bottomChildren bottom
    bottom@(IBottom1 _ _ _)     -> consumeMap $ mut_fn $ bottomChildren bottom
    branch@(Branch _ _ _ _ _)   -> mutate search_key mut_fn branch
    branch@(IBranch0 _ _)       -> mutate search_key mut_fn branch
    branch@(IBranch1 _ _ _)     -> mutate search_key mut_fn branch
    branch@(IBranch2 _ _ _ _ _) -> mutate search_key mut_fn branch
  where

  mutate :: (Ord k, StableKey k)
         => k
         -> (Map k v -> Map k v)
         -> Tree (S d) c k v
         -> ([Tree (S d) Complete k v], Maybe (Tree (S d) Incomplete k v))
  mutate key fn b =
    case selectNode key b of
      (Left (before, incomplete)) ->
        let (mut_before, mut_minc) = mutateBottom key fn incomplete
        in consumeBranches' (before++mut_before) mut_minc
      (Right (before, tree, after, mincomplete)) ->
        let (mut_before, mut_minc)       = mutateBottom key fn tree
            (merged_before, merged_minc) = merge (before++mut_before)
                                           mut_minc
                                           after
                                           mincomplete
        in consumeBranches' merged_before merged_minc

