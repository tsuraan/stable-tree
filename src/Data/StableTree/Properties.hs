{-# LANGUAGE GADTs #-}
module Data.StableTree.Properties
( getKey
, completeKey
, size
, lookup
, treeContents
, toMap
, stableChildren
, bottomChildren
, branchChildren
, selectNode
) where

import qualified Data.StableTree.Key as Key
import Data.StableTree.Types

import qualified Data.Map as Map
import Control.Arrow  ( second )
import Data.Map       ( Map )

import Prelude hiding ( lookup )

-- |Get the key of the first entry in this branch. If the branch is empty,
-- returns Nothing.
getKey :: Tree d c k v -> Maybe k
getKey (Bottom _ (k,_) _ _ _)       = Just $ Key.unwrap k
getKey (IBottom0 _ Nothing)         = Nothing
getKey (IBottom0 _ (Just (k,_)))    = Just $ Key.unwrap k
getKey (IBottom1 _ (k,_) _ _)       = Just $ Key.unwrap k
getKey (Branch _ _ (k,_,_) _ _ _)   = Just $ Key.unwrap k
getKey (IBranch0 _ _ (k,_,_))       = Just $ Key.unwrap k
getKey (IBranch1 _ _ (k,_,_) _)     = Just $ Key.unwrap k
getKey (IBranch2 _ _ (k,_,_) _ _ _) = Just $ Key.unwrap k

-- |Get the key of the first entry in this complete branch. This function is
-- total.
completeKey :: Tree d Complete k v -> k
completeKey (Bottom _ (k,_) _ _ _)     = Key.unwrap k
completeKey (Branch _ _ (k,_,_) _ _ _) = Key.unwrap k

-- |Get the total number of k/v pairs in the tree
size :: StableTree k v -> ValueCount
size = getValueCount

-- |Get the value associated with the given key, or Nothing if there is no
-- value for the key.
lookup :: Ord k => k -> StableTree k v -> Maybe v
lookup key tree =
  case tree of
    StableTree_I i -> lookup' key i
    StableTree_C c -> lookup' key c
  where
  lookup' :: Ord k => k -> Tree d c k v -> Maybe v
  lookup' k t =
    case t of
      Bottom _ _ _ _ _     -> Map.lookup k $ bottomChildren t
      IBottom0 _ _         -> Map.lookup k $ bottomChildren t
      IBottom1 _ _ _ _     -> Map.lookup k $ bottomChildren t
      Branch _ _ _ _ _ _   -> lookup'' k t
      IBranch0 _ _ _       -> lookup'' k t
      IBranch1 _ _ _ _     -> lookup'' k t
      IBranch2 _ _ _ _ _ _ -> lookup'' k t

  lookup'' :: Ord k => k -> Tree (S d) c k v -> Maybe v
  lookup'' k t =
    case selectNode k t of
      Left (_, inc) -> lookup' k inc
      Right (_, comp, _, _) -> lookup' k comp

-- |Convert an entire Tree into a k/v map.
treeContents :: Ord k => Tree d c k v -> Map k v
treeContents t =
  case t of
    (Bottom _ _ _ _ _)     -> bottomChildren t
    (IBottom0 _ _)         -> bottomChildren t
    (IBottom1 _ _ _ _)     -> bottomChildren t
    (Branch _ _ _ _ _ _)   -> recur $ branchChildren t
    (IBranch0 _ _ _)       -> recur $ branchChildren t
    (IBranch1 _ _ _ _)     -> recur $ branchChildren t
    (IBranch2 _ _ _ _ _ _) -> recur $ branchChildren t

  where
  recur :: Ord k
        => ( Map k (ValueCount, Tree d Complete k v)
           , Maybe (k, ValueCount, Tree d Incomplete k v))
        -> Map k v
  recur x =
    case x of
      ( completes, Nothing) ->
        Map.unions $ map (treeContents . snd) $ Map.elems completes
      ( completes, Just (_k, _c, iv)) ->
        Map.unions $ treeContents iv:map (treeContents . snd) (Map.elems completes)

-- |Convert a 'StableTree' into a normal key/value Map
toMap :: Ord k => StableTree k v -> Map k v
toMap (StableTree_I i) = treeContents i
toMap (StableTree_C c) = treeContents c

-- |Either get the StableTree "children" of a 'StableTree', or get the
-- key/value map if the tree is already a bottom.
stableChildren :: Ord k
               => StableTree k v
               -> Either (Map k v) (Map k (ValueCount, StableTree k v))
stableChildren tree =
  case tree of
    StableTree_I i -> stableChildren' i
    StableTree_C c -> stableChildren' c
  where
  stableChildren' :: Ord k
                  => Tree d c k v
                  -> Either (Map k v) (Map k (ValueCount, StableTree k v))
  stableChildren' t =
    case t of
      (Bottom _ _ _ _ _)     -> Left $ bottomChildren t
      (IBottom0 _ _)         -> Left $ bottomChildren t
      (IBottom1 _ _ _ _)     -> Left $ bottomChildren t
      (Branch _ _ _ _ _ _)   -> Right $ branchChildren' t
      (IBranch0 _ _ _)       -> Right $ branchChildren' t
      (IBranch1 _ _ _ _)     -> Right $ branchChildren' t
      (IBranch2 _ _ _ _ _ _) -> Right $ branchChildren' t

  branchChildren' :: Ord k
                  => Tree (S d) c k v
                  -> Map k (ValueCount, StableTree k v)
  branchChildren' t =
    let (compMap, minc) = branchChildren t
        stableMap       = Map.map (second StableTree_C) compMap
        fullMap         = case minc of
                            Nothing ->
                              stableMap
                            Just (k, c, i) ->
                              Map.insert k (c, StableTree_I i) stableMap
    in fullMap

-- |Non-recursive function to simply get the immediate children of the given
-- branch. This will either give the key/value map of a Bottom, or the key/tree
-- map of a non-bottom branch.
bottomChildren :: Ord k
               => Tree Z c k v
               -> Map k v
bottomChildren (Bottom _ (k1,v1) (k2,v2) terms (kt,vt)) =
  let terms' = Map.mapKeys Key.fromKey terms
      conts  = Map.insert (Key.unwrap k1) v1
             $ Map.insert (Key.unwrap k2) v2
             $ Map.insert (Key.fromKey kt) vt
             terms'
  in conts
bottomChildren (IBottom0 _ Nothing) =
  Map.empty
bottomChildren (IBottom0 _ (Just (k,v))) =
  Map.singleton (Key.unwrap k) v
bottomChildren (IBottom1 _ (k1,v1) (k2,v2) terms) =
  let terms' = Map.mapKeys Key.fromKey terms
      conts  = Map.insert (Key.unwrap k1) v1
             $ Map.insert (Key.unwrap k2) v2
             terms'
  in conts

-- |Get the 'Tree's stored under the given Tree. The Tree type prevents this
-- function from being called on bottom Trees.
branchChildren :: Ord k
               => Tree (S d) c k v
               -> ( Map k (ValueCount, Tree d Complete k v)
                  , Maybe (k, ValueCount, Tree d Incomplete k v))
branchChildren (Branch _ _d (k1,c1,v1) (k2,c2,v2) terms (kt,ct,vt)) =
  let terms' = Map.mapKeys Key.fromKey terms
      conts  = Map.insert (Key.unwrap k1) (c1,v1)
             $ Map.insert (Key.unwrap k2) (c2,v2)
             $ Map.insert (Key.fromKey kt) (ct,vt)
             terms'
  in (conts, Nothing)
branchChildren (IBranch0 _ _d (ik,ic,iv)) =
  (Map.empty, Just (Key.unwrap ik, ic, iv))
branchChildren (IBranch1 _ _d (k1,c1,v1) mIncomplete) =
  ( Map.singleton (Key.unwrap k1) (c1,v1)
  , mIncomplete >>= (\(k,c,v) -> return (Key.unwrap k,c,v)))
branchChildren (IBranch2 _ _d (k1,c1,v1) (k2,c2,v2) terms mIncomplete) =
  let terms' = Map.mapKeys Key.fromKey terms
      conts  = Map.insert (Key.unwrap k1) (c1,v1)
             $ Map.insert (Key.unwrap k2) (c2,v2)
             terms'
  in (conts, mIncomplete >>= \(k,c,v) -> return (Key.unwrap k, c, v))

selectNode :: Ord k
           => k
           -> Tree (S d) c k v
           -> Either ( [Tree d Complete k v], Tree d Incomplete k v )
                     ( [Tree d Complete k v], Tree d Complete k v
                     , [Tree d Complete k v], Maybe (Tree d Incomplete k v) )
selectNode key branch =
  let (completes, minc)  = branchChildren branch
      assocs             = Map.toAscList completes
      minc_t             = Prelude.fmap (\(_, _, t) -> t) minc
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

