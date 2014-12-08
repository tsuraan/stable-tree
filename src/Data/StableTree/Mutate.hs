{-# LANGUAGE LambdaCase, GADTs #-}
module Data.StableTree.Mutate
( insert
, delete
, fmap
) where

import Data.StableTree.Types
import Data.StableTree.Build      ( consume, consumeBranches', consumeMap, merge )
import Data.StableTree.Properties ( bottomChildren, selectNode )

import qualified Data.Map as Map
import Data.Map       ( Map )
import Data.Serialize ( Serialize )

import qualified Prelude
import Prelude hiding ( fmap )

-- |Insert a key/value into a 'StableTree'. If the key exists, its existing
-- value is overwritten.
insert :: (Ord k, Serialize k, Serialize v)
       => k -> v -> StableTree k v -> StableTree k v
insert k v (StableTree_C c) = (uncurry consume) $ insert' k v c
insert k v (StableTree_I i) = (uncurry consume) $ insert' k v i

-- |Remove a key from the 'StableTree'. If the key is not found, the tree is
-- returned unchanged.
delete :: (Ord k, Serialize k, Serialize v)
       => k -> StableTree k v -> StableTree k v
delete k (StableTree_C c) = (uncurry consume) $ delete' k c
delete k (StableTree_I i) = (uncurry consume) $ delete' k i

-- |Same as 'insert', but works on a 'Tree', and returns a list of completes
-- and a maybe incomplete instead of returning something that probably can't be
-- expressed in Haskell's type system.
insert' :: (Ord k, Serialize k, Serialize v)
        => k
        -> v
        -> Tree d c k v
        -> ([Tree d Complete k v], Maybe (Tree d Incomplete k v))
insert' k v = mutateBottom k $ Map.insert k v

-- |Same as 'delete', but works on a 'Tree', and returns a list of completes
-- and a maybe incomplete instead of returning something that probably can't be
-- expressed in Haskell's type system.
delete' :: (Ord k, Serialize k, Serialize v)
        => k
        -> Tree d c k v
        -> ([Tree d Complete k v], Maybe (Tree d Incomplete k v))
delete' k = mutateBottom k $ Map.delete k

-- |Find the 'Tree Z' instance that should contain the given key, and call the
-- given function on its contents. Once that's done, splice the result back
-- into a new tree, which will probably be really similar to the original, but
-- have the desired changes applied.
mutateBottom :: (Ord k, Serialize k, Serialize v)
             => k
             -> (Map k v -> Map k v)
             -> Tree d c k v
             -> ([Tree d Complete k v], Maybe (Tree d Incomplete k v))
mutateBottom search_key mut_fn = \case
    bottom@(Bottom _ _ _ _ _)     -> consumeMap $ mut_fn $ bottomChildren bottom
    bottom@(IBottom0 _ _)         -> consumeMap $ mut_fn $ bottomChildren bottom
    bottom@(IBottom1 _ _ _ _)     -> consumeMap $ mut_fn $ bottomChildren bottom
    branch@(Branch _ _ _ _ _ _)   -> mutate search_key mut_fn branch
    branch@(IBranch0 _ _ _)       -> mutate search_key mut_fn branch
    branch@(IBranch1 _ _ _ _)     -> mutate search_key mut_fn branch
    branch@(IBranch2 _ _ _ _ _ _) -> mutate search_key mut_fn branch
  where

  mutate :: (Ord k, Serialize k, Serialize v)
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

class SerialFunctor f where
  -- |Same as the 'fmap' instance of 'Functor', but with the restriction that
  -- the input and output of the mutation function must be 'Serialize'-able.
  -- Using the real instance would be really cool, but we need that Serialize
  -- instance.
  fmap :: (Serialize a, Serialize b) => (a -> b) -> f a -> f b

instance (Ord k, Serialize k) => SerialFunctor (Tree d c k) where
  fmap fn (Bottom _ (k1, v1) (k2, v2) nonterms (kt, vt)) =
    mkBottom (k1, fn v1) (k2, fn v2) (Map.map fn nonterms) (kt, fn vt)
  fmap fn (IBottom0 _ mpair) =
    mkIBottom0 (Prelude.fmap (\(k,v) -> (k, fn v)) mpair)
  fmap fn (IBottom1 _ (k1, v1) (k2, v2) nonterms) =
    mkIBottom1 (k1, fn v1) (k2, fn v2) (Map.map fn nonterms)
  fmap fn (Branch _ d (k1, c1, t1) (k2, c2, t2) nonterms (kt, ct, tt)) =
    mkBranch d
             (k1, c1, fmap fn t1)
             (k2, c2, fmap fn t2)
             (Map.map (\(c,t) -> (c, fmap fn t)) nonterms)
             (kt, ct, fmap fn tt)
  fmap fn (IBranch0 _ d (k1, c1, t1)) =
    mkIBranch0 d
               (k1, c1, fmap fn t1)
  fmap fn (IBranch1 _ d (k1, c1, t1) mtriple) =
    mkIBranch1 d
               (k1, c1, fmap fn t1)
               (Prelude.fmap (\(k, c, t) -> (k, c, fmap fn t)) mtriple)
  fmap fn (IBranch2 _ d (k1, c1, t1) (k2, c2, t2) nonterms mtriple) =
    mkIBranch2 d
               (k1, c1, fmap fn t1)
               (k2, c2, fmap fn t2)
               (Map.map (\(c, t) -> (c, fmap fn t)) nonterms)
               (Prelude.fmap (\(k, c, t) -> (k, c, fmap fn t)) mtriple)

instance (Ord k, Serialize k) => SerialFunctor (StableTree k) where
  fmap fn (StableTree_I i) = StableTree_I $ fmap fn i
  fmap fn (StableTree_C c) = StableTree_C $ fmap fn c

