{-# LANGUAGE LambdaCase, GADTs #-}
module Data.StableTree.Mutate
( insert
, delete
) where

import Data.StableTree.Types
import Data.StableTree.Build      ( consumeBranches', consumeMap, merge )
import Data.StableTree.Properties ( branchChildren , bottomChildren )

import qualified Data.Map as Map
import Data.Map       ( Map )
import Data.Serialize ( Serialize )
import Data.Ord       ( comparing )
import Data.List      ( sortBy )

insert :: (Ord k, Serialize k, Serialize v)
       => k -> v -> StableTree k v -> StableTree k v
insert k v (StableTree_C c) = (uncurry vroom) $ insert' k v c
insert k v (StableTree_I i) = (uncurry vroom) $ insert' k v i

delete :: (Ord k, Serialize k, Serialize v)
       => k -> StableTree k v -> StableTree k v
delete k (StableTree_C c) = (uncurry vroom) $ delete' k c
delete k (StableTree_I i) = (uncurry vroom) $ delete' k i

vroom :: (Ord k, Serialize k, Serialize v)
      => [Tree d Complete k v] -> Maybe (Tree d Incomplete k v)
      -> StableTree k v
vroom [complete] Nothing   = StableTree_C complete
vroom [] (Just incomplete) = StableTree_I incomplete
vroom cs minc = (uncurry vroom) $ consumeBranches' cs minc

insert' :: (Ord k, Serialize k, Serialize v)
        => k
        -> v
        -> Tree d c k v
        -> ([Tree d Complete k v], Maybe (Tree d Incomplete k v))
insert' k v = mutateBottom k $ Map.insert k v

delete' :: (Ord k, Serialize k, Serialize v)
        => k
        -> Tree d c k v
        -> ([Tree d Complete k v], Maybe (Tree d Incomplete k v))
delete' k = mutateBottom k $ Map.delete k

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

  selectNode :: (Ord k, Serialize k, Serialize v)
             => k
             -> Tree (S d) c k v
             -> Either ( [Tree d Complete k v], Tree d Incomplete k v )
                       ( [Tree d Complete k v], Tree d Complete k v
                       , [Tree d Complete k v], Maybe (Tree d Incomplete k v) )
  selectNode key branch =
    let (completes, minc)  = branchChildren branch
        assocs             = sortBy (comparing fst) (Map.assocs completes)
        minc_t             = fmap (\(_, _, t) -> t) minc
        test               = \(k, _) -> k <= key
        -- begin_k is every tree whose lowest key is leq to the given key
        (begin_k, after_k) = span test assocs
        begin              = [ t | (_, (_, t)) <- begin_k ]
        after              = [ t | (_, (_, t)) <- after_k ]
    in case (reverse begin, after, minc) of
      ([], [], Nothing) ->                  -- empty branch
        error "this is totally unreachable. branches are _not_ empty"
      ([], [], Just (_, _, i)) ->           -- only choice is the incomplete
        Left ([], i)
      (_, [], Just (k, _, t)) | k <= key -> -- key goes with the incomplete
        Left (begin, t)
      ([], t:rest, _) ->                    -- key is before everything
        Right ([], t, rest, minc_t)
      (t:rest, _, _) ->                     -- key goes with "t"
        Right (reverse rest, t, after, minc_t)

