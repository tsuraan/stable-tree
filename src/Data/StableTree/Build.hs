{-# LANGUAGE LambdaCase, OverloadedStrings, GADTs, ExistentialQuantification #-}
-- |
-- Module    : Data.StableTree.Build
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- This is the core implementation of the stable tree. The primary functions
-- exported by this module are 'nextBottom' and 'nextBranch', which gather
-- values or lower-level 'Tree's into 'Tree's of the next level.
--
-- This module is fairly esoteric. "Data.StableTree" or "Data.StableTree.IO"
-- are probably what you actually want to be using.
module Data.StableTree.Build
( NextBranch(..)
, nextBottom
, nextBranch
) where

import qualified Data.StableTree.Key as Key
import Data.StableTree.Key      ( SomeKey(..) )
import Data.StableTree.Types

import qualified Data.Map as Map
import Control.Arrow  ( first, second )
import Data.Map       ( Map )
import Data.Serialize ( Serialize )

-- |Wrap up some of a k/v map into a 'Tree'. A 'Right' result gives a complete
-- tree and the map updated to not have the key/values that went into that
-- tree. A 'Left' result gives an incomplete tree that contains everything that
-- the given map contained.
nextBottom :: (Ord k, Serialize k, Serialize v)
           => Map k v
           -> Either (Tree Z Incomplete k v)
                     (Tree Z Complete k v, Map k v)
nextBottom values =
  case Map.minViewWithKey values >>= return . second Map.minViewWithKey of
    Just (f1, Just (f2, remain)) ->
      go (first Key.wrap f1) (first Key.wrap f2) Map.empty remain
    partial ->
      -- this is a bit odd, because I couldn't come up with a better way to tie
      -- the type of the Nothing to the type of the Just, so that
      -- iBottom0ObjectID would be satisfied.
      let m = case partial of
                Nothing -> Nothing
                Just ((k,v), Nothing) -> Just (Key.wrap k, v)
                _ ->
                  error "This is just here to satisfy a broken exhaustion check"
          b = fixObjectID $ IBottom0 undefined m
      in Left b

  where
  go f1 f2 accum remain =
    case Map.minViewWithKey remain of
      Nothing ->
        Left $ fixObjectID $ IBottom1 undefined f1 f2 accum
      Just ((k, v), remain') ->
        case Key.wrap k of
          SomeKey_N nonterm ->
            go f1 f2 (Map.insert nonterm v accum) remain'
          SomeKey_T term ->
            Right (fixObjectID $ Bottom undefined f1 f2 accum (term, v), remain')

data NextBranch d k v
  = Empty
  | Final (Tree (S d) Incomplete k v)
  | More  (Tree (S d) Complete k v) (Map k (Tree d Complete k v))

-- |Generate a parent for a k/Tree map. A 'Right' result gives a complete tree
-- and the map updated to not have the key/trees that went into that tree. A
-- 'Left' result gives an incomplete tree that contains everything that the
-- given map contained.
nextBranch :: (Ord k, Serialize k, Serialize v)
           => Map k (Tree d Complete k v)
           -> Maybe (k, Tree d Incomplete k v)
           -> NextBranch d k v
nextBranch branches mIncomplete =
  let freebies = Map.minViewWithKey branches
                 >>= return . second Map.minViewWithKey
  in case freebies of
    Nothing -> 
      case mIncomplete of
        Nothing ->
          Empty
        Just (ik, iv) ->
          let tup = (Key.wrap ik, getValueCount iv, iv)
              b   = fixObjectID $ IBranch0 undefined depth tup
          in Final b
    Just ((k,v), Nothing) ->
      let tup = (Key.wrap k, getValueCount v, v)
          may = wrapMKey mIncomplete
      in Final $ fixObjectID $ IBranch1 undefined depth tup may
    Just (f1, Just (f2, remain)) ->
      go (wrapKey f1) (wrapKey f2) Map.empty remain

  where
  go f1 f2 accum remain =
    let popd = Map.minViewWithKey remain >>= return . first wrapKey
    in case popd of
      Nothing ->
        let may = wrapMKey mIncomplete
        in Final $ fixObjectID $ IBranch2 undefined depth f1 f2 accum may 
      Just ((SomeKey_T term,c,v), remain') ->
        let tup = (term, c, v)
        in More (fixObjectID $ Branch undefined depth f1 f2 accum tup) remain'
      Just ((SomeKey_N nonterm,c,v), remain') ->
        go f1 f2 (Map.insert nonterm (c,v) accum) remain'

  wrapKey (k,v) = (Key.wrap k, getValueCount v, v)

  wrapMKey = (>>=return . wrapKey)

  depth = case Map.elems branches of
    [] ->
      case mIncomplete of
        Nothing -> 1
        Just (_, v) -> 1 + getDepth v
    elems ->
      let depths@(f:r) = map getDepth elems
          (best, rest) = case mIncomplete of
                          Nothing -> (f, r)
                          Just (_, v) -> (getDepth v, depths)
      in if all (==best) rest
        then 1 + best
        else error "Depth mismatch in nextBranch"

