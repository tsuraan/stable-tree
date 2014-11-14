{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module    : Data.StableTree.Conversion
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- Functions for converting between Tree and Fragment types
module Data.StableTree.Conversion
( toFragments
, topFragment
, fromFragments
) where

import Data.StableTree.Fragment
import Data.StableTree.Tree

import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Arrow  ( second )
import Data.Map       ( Map )
import Data.ObjectID  ( ObjectID )
import Data.Serialize ( Serialize )
import Data.Text      ( Text )

-- |Convert a 'StableTree' 'Tree' into a list of storable 'Fragment's. The
-- resulting list is guaranteed to be in an order where each 'Fragment' will be
-- seen after all its children.
toFragments :: Ord k => StableTree k v -> [(ObjectID, Fragment k v)]
toFragments tree =
  case stableNodeContents tree of
    Right bottom -> [(getObjectID tree, FragmentBottom bottom)]
    Left ( completes, mIncomplete ) ->
      let depth    = getDepth tree
          cont     = Map.map (second getObjectID) completes
          cont'    = case mIncomplete of
                       Nothing -> cont
                       Just (key,c,t) -> Map.insert key (c,getObjectID t) cont
          this     = FragmentBranch depth cont'
          below  = concat $ map (toFragments . snd) $ Map.elems completes
          below' = case mIncomplete of
                     Nothing -> below
                     Just (_,_,t) -> below ++ toFragments t
      in below' ++ [(getObjectID tree, this)]

-- |Get the root fragment for the given tree. This will give the same value as
-- `snd . last . toFragments`, but does a lot less work.
topFragment :: Ord k => StableTree k v -> (Fragment k v)
topFragment tree =
  case stableNodeContents tree of
    Right bottom -> FragmentBottom bottom
    Left ( completes, mIncomplete ) ->
      let depth    = getDepth tree
          cont     = Map.map (second getObjectID) completes
          cont'    = case mIncomplete of
                       Nothing -> cont
                       Just (key,c,t) -> Map.insert key (c,getObjectID t) cont
      in FragmentBranch depth cont'

-- |Recover a 'Tree' from a single 'Fragment' and a map of the fragments as
-- returned from 'toFragments'. If the fragment set was already stored, it is
-- the caller's responsibility to load all the child fragments into a map
-- (probably involving finding children using the fragmentChildren field of the
-- Fragment type).
fromFragments :: (Ord k, Serialize k, Serialize v)
              => Map ObjectID (Fragment k v)
              -> Fragment k v
              -> Either Text (StableTree k v)
fromFragments _ (FragmentBottom assocs) =
  case nextBottom assocs of
    Left i -> Right $ StableTree_I i
    Right (c, remain)
      | Map.null remain -> Right $ StableTree_C c
      | otherwise       -> Left "Fragment had leftovers!?"
fromFragments loaded (FragmentBranch depth children) =
  

fragsToMap :: Ord k
           => Map ObjectID (Fragment k v)
           -> Fragment k v
           -> Either Text (Map k v)
fragsToMap loaded = go Map.empty
  where
  go accum (FragmentBottom m) = Right $ Map.union accum m
  go accum (FragmentBranch _ children) =
    go' accum $ map snd $ Map.elems children

  go' accum [] = Right accum
  go' accum (first:rest) =
    case Map.lookup first loaded of
      Nothing -> notFound first
      Just frag -> do
        nxt <- go accum frag
        go' nxt rest

  notFound objectid =
    Left $ Text.append "Failed to find Fragment with ID "
                       (Text.pack $ show objectid)
