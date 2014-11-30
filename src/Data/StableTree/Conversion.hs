{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module    : Data.StableTree.Conversion
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- Functions for converting between Tree and Fragment types
module Data.StableTree.Conversion
( toFragments
, fromFragments
, fragsToMap
, fromMap
) where

import Data.StableTree.Properties ( stableChildren )
import Data.StableTree.Build      ( consume, consumeMap )
import Data.StableTree.Types

import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Map       ( Map )
import Data.ObjectID  ( ObjectID )
import Data.Serialize ( Serialize )
import Data.Text      ( Text )

-- |Convert a 'StableTree' 'Tree' into a list of storable 'Fragment's. The
-- resulting list is guaranteed to be in an order where each 'Fragment' will be
-- seen after all its children.
toFragments :: Ord k => StableTree k v -> [(ObjectID, Fragment k v)]
toFragments tree =
  let oid    = getObjectID tree
      frag   = makeFragment tree
  in case stableChildren tree of
    Left _ -> [(oid, frag)]
    Right children ->
      let below  = concat $ map (toFragments . snd) $ Map.elems children
      in below ++ [(oid, frag)]

-- |Recover a 'Tree' from a single 'Fragment' and a map of the fragments as
-- returned from 'toFragments'. If the fragment set was already stored, it is
-- the caller's responsibility to load all the child fragments into a map
-- (probably involving finding children using the fragmentChildren field of the
-- Fragment type).
fromFragments :: (Ord k, Serialize k, Serialize v)
              => Map ObjectID (Fragment k v)
              -> Fragment k v
              -> Either Text (StableTree k v)
fromFragments loaded top = do
  (complete, mincomplete) <- fragsToBottoms loaded top
  return $ consume complete mincomplete

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

fragsToBottoms :: (Ord k, Serialize k, Serialize v)
               => Map ObjectID (Fragment k v)
               -> Fragment k v
               -> Either Text ( [Tree Z Complete k v]
                              , Maybe (Tree Z Incomplete k v))
fragsToBottoms _ (FragmentBottom m) = Right $ consumeMap m
fragsToBottoms frags top =
  let content = fragmentChildren top
      asList  = Map.toAscList content
      oids    = map (snd.snd) asList
  in go oids
  where
  go []  = Right ([], Nothing)
  go [oid] =
    case Map.lookup oid frags of
      Nothing -> Left "Failed to lookup a fragment"
      Just frag -> fragsToBottoms frags frag
  go (oid:oids) =
    case Map.lookup oid frags of
      Nothing -> Left "Failed to lookup a fragment"
      Just frag ->
        case fragsToBottoms frags frag of
          Left err -> Left err
          Right (completes, Nothing) ->
            case go oids of
              Left err -> Left err
              Right (nxtC, nxtE) ->
                Right (completes ++ nxtC, nxtE)
          _ ->
            Left "Got an Incomplete bottom in a non-terminal position"

fromMap :: (Ord k, Serialize k, Serialize v) => Map k v -> StableTree k v
fromMap = (uncurry consume) . consumeMap
