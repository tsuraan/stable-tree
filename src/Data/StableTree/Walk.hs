{-# LANGUAGE GADTs #-}
-- |
-- Module    : Data.StableTree.Walk
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- Functions for iterating over a StableTree. Currently just folds, but
-- convenience functions for maps will probably be added at some point.
module Data.StableTree.Walk
( foldM
, foldr
, foldl
) where

import Data.StableTree.Properties ( bottomChildren, branchChildren )
import Data.StableTree.Types

import qualified Control.Monad as M
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Prelude
import Prelude hiding ( foldr, foldl )

-- |Monadic fold over a StableTree in the style of `Control.Monad.foldM`.
foldM :: (Monad m, Ord k)
      => (a -> k -> v -> m a) -> a -> StableTree k v -> m a
foldM fn a0 tree =
  case tree of
    StableTree_I i -> foldM' fn a0 i
    StableTree_C c -> foldM' fn a0 c
  where
  foldM' :: (Monad m, Ord k)
         => (a -> k -> v -> m a) -> a -> Tree d c k v -> m a
  foldM' f accum t =
    case t of
      Bottom _ _ _ _     -> bottom f accum t
      IBottom0 _         -> bottom f accum t
      IBottom1 _ _ _     -> bottom f accum t
      Branch _ _ _ _ _   -> branch f accum t
      IBranch0 _ _       -> branch f accum t
      IBranch1 _ _ _     -> branch f accum t
      IBranch2 _ _ _ _ _ -> branch f accum t

  bottom :: (Monad m, Ord k)
         => (a -> k -> v -> m a) -> a -> Tree Z c k v -> m a
  bottom f accum t =
    let children  = Map.assocs $ bottomChildren t
        f' a (k,v) = f a k v
    in M.foldM f' accum children

  branch :: (Monad m, Ord k)
         => (a -> k -> v -> m a) -> a -> Tree (S d) c k v -> m a
  branch f accum t = do
    let (compMap, mi) = branchChildren t
    accum' <- M.foldM (foldM' f) accum (map snd $ Map.elems compMap)
    case mi of
      Nothing -> return accum'
      Just i  -> foldM' f accum' (third i)

  third (_, _, t) = t

-- |Right fold over a StableTree. Similar to `Data.Map.foldrWithKey`.
foldr :: Ord k => (k -> v -> a -> a) -> a -> StableTree k v -> a
foldr fn a0 tree =
  case tree of
    StableTree_I i -> foldr' fn a0 i
    StableTree_C c -> foldr' fn a0 c
  where
  foldr' :: Ord k => (k -> v -> a -> a) -> a -> Tree d c k v -> a
  foldr' f accum t =
    case t of
      Bottom _ _ _ _     -> bottom f accum t
      IBottom0 _         -> bottom f accum t
      IBottom1 _ _ _     -> bottom f accum t
      Branch _ _ _ _ _   -> branch f accum t
      IBranch0 _ _       -> branch f accum t
      IBranch1 _ _ _     -> branch f accum t
      IBranch2 _ _ _ _ _ -> branch f accum t

  bottom :: Ord k => (k -> v -> a -> a) -> a -> Tree Z c k v -> a
  bottom f accum t =
    let children  = Map.assocs $ bottomChildren t
        f' (k,v) a = f k v a
    in Prelude.foldr f' accum children

  branch :: Ord k => (k -> v -> a -> a) -> a -> Tree (S d) c k v -> a
  branch f accum t =
    let (compMap, mi) = branchChildren t
        elems         = map snd $ Map.elems compMap
        accum'        = case mi of
                          Nothing -> accum
                          Just i  -> foldr' f accum (third i)
    in Prelude.foldr (flip $ foldr' f) accum' elems

  third (_, _, t) = t

-- |Left fold over a StableTree. Similar to `Data.Map.foldlWithKey`.
foldl :: Ord k => (a -> k -> v -> a) -> a -> StableTree k v -> a
foldl fn a0 tree =
  case tree of
    StableTree_I i -> foldl' fn a0 i
    StableTree_C c -> foldl' fn a0 c
  where
  foldl' :: Ord k => (a -> k -> v -> a) -> a -> Tree d c k v -> a
  foldl' f accum t =
    case t of
      Bottom _ _ _ _     -> bottom f accum t
      IBottom0 _         -> bottom f accum t
      IBottom1 _ _ _     -> bottom f accum t
      Branch _ _ _ _ _   -> branch f accum t
      IBranch0 _ _       -> branch f accum t
      IBranch1 _ _ _     -> branch f accum t
      IBranch2 _ _ _ _ _ -> branch f accum t

  bottom :: Ord k => (a -> k -> v -> a) -> a -> Tree Z c k v -> a
  bottom f accum t =
    let children  = Map.assocs $ bottomChildren t
        f' a (k,v) = f a k v
    in List.foldl' f' accum children

  branch :: Ord k => (a -> k -> v -> a) -> a -> Tree (S d) c k v -> a
  branch f accum t =
    let (compMap, mi) = branchChildren t
        elems         = map snd $ Map.elems compMap
        accum'        = List.foldl' (foldl' f) accum elems
    in case mi of
        Nothing -> accum'
        Just i  -> foldl' f accum' (third i)

  third (_, _, t) = t

