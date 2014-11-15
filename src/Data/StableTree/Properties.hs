{-# LANGUAGE GADTs #-}
module Data.StableTree.Properties
( getKey
, completeKey
, treeContents
, bottomContents
, branchContents
) where

import qualified Data.StableTree.Key as Key
import Data.StableTree.Types

import qualified Data.Map as Map
import Data.Map       ( Map )

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

-- |Get the key of the fist entry in this complete branch. This function is
-- total.
completeKey :: Tree d Complete k v -> k
completeKey (Bottom _ (k,_) _ _ _)     = Key.unwrap k
completeKey (Branch _ _ (k,_,_) _ _ _) = Key.unwrap k

-- |Convert an entire Tree into a k/v map.
treeContents :: Ord k => Tree d c k v -> Map k v
treeContents t =
  case t of
    (Bottom _ _ _ _ _)     -> bottomContents t
    (IBottom0 _ _)         -> bottomContents t
    (IBottom1 _ _ _ _)     -> bottomContents t
    (Branch _ _ _ _ _ _)   -> recur $ branchContents t
    (IBranch0 _ _ _)       -> recur $ branchContents t
    (IBranch1 _ _ _ _)     -> recur $ branchContents t
    (IBranch2 _ _ _ _ _ _) -> recur $ branchContents t

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

-- |Non-recursive function to simply get the immediate children of the given
-- branch. This will either give the key/value map of a Bottom, or the key/tree
-- map of a non-bottom branch.
bottomContents :: Ord k
               => Tree Z c k v
               -> Map k v
bottomContents (Bottom _ (k1,v1) (k2,v2) terms (kt,vt)) =
  let terms' = Map.mapKeys Key.fromKey terms
      conts  = Map.insert (Key.unwrap k1) v1
             $ Map.insert (Key.unwrap k2) v2
             $ Map.insert (Key.fromKey kt) vt
             terms'
  in conts
bottomContents (IBottom0 _ Nothing) =
  Map.empty
bottomContents (IBottom0 _ (Just (k,v))) =
  Map.singleton (Key.unwrap k) v
bottomContents (IBottom1 _ (k1,v1) (k2,v2) terms) =
  let terms' = Map.mapKeys Key.fromKey terms
      conts  = Map.insert (Key.unwrap k1) v1
             $ Map.insert (Key.unwrap k2) v2
             terms'
  in conts

branchContents :: Ord k
               => Tree (S d) c k v
               -> ( Map k (ValueCount, Tree d Complete k v)
                  , Maybe (k, ValueCount, Tree d Incomplete k v))
branchContents (Branch _ _d (k1,c1,v1) (k2,c2,v2) terms (kt,ct,vt)) =
  let terms' = Map.mapKeys Key.fromKey terms
      conts  = Map.insert (Key.unwrap k1) (c1,v1)
             $ Map.insert (Key.unwrap k2) (c2,v2)
             $ Map.insert (Key.fromKey kt) (ct,vt)
             terms'
  in (conts, Nothing)
branchContents (IBranch0 _ _d (ik,ic,iv)) =
  (Map.empty, Just (Key.unwrap ik, ic, iv))
branchContents (IBranch1 _ _d (k1,c1,v1) mIncomplete) =
  ( Map.singleton (Key.unwrap k1) (c1,v1)
  , mIncomplete >>= (\(k,c,v) -> return (Key.unwrap k,c,v)))
branchContents (IBranch2 _ _d (k1,c1,v1) (k2,c2,v2) terms mIncomplete) =
  let terms' = Map.mapKeys Key.fromKey terms
      conts  = Map.insert (Key.unwrap k1) (c1,v1)
             $ Map.insert (Key.unwrap k2) (c2,v2)
             terms'
  in (conts, mIncomplete >>= \(k,c,v) -> return (Key.unwrap k, c, v))

