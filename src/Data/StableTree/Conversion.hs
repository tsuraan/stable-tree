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
toFragments :: Ord k => Tree c k v -> [(ObjectID, Fragment k v)]
toFragments tree =
  case branchContents tree of
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
topFragment :: Ord k => Tree c k v -> (Fragment k v)
topFragment tree =
  case branchContents tree of
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
              -> Either Text (Either (Tree Incomplete k v)
                                     (Tree Complete k v))
fromFragments _ (FragmentBottom assocs) =
  case nextBottom assocs of
    Left i -> Right $ Left i
    Right (c, remain)
      | Map.null remain -> Right $ Right c
      | otherwise       -> Left "Fragment had leftovers!?"
fromFragments loaded (FragmentBranch depth children) =
  case readChildren Map.empty (Map.toAscList children) of
    Left err -> Left err
    Right (tmap, minc) ->
      case nextBranch tmap minc of
        Left i -> Right $ Left i
        Right (c, remain)
          | Map.null remain && getDepth c == depth -> Right $ Right c
          | otherwise       -> Left "Fragment rebuild failed"

  where
  readChildren _ [] = Left "Invalid empty branch"
  readChildren accum [(key,(cnt,oid))] =
    case Map.lookup oid loaded of
      Nothing -> Left $ cannotFind oid
      Just frag ->
        case fromFragments loaded frag of
          Left err                   -> Left err
          Right (Right c)
            | getValueCount c == cnt -> Right (Map.insert key c accum, Nothing)
            | otherwise              -> Left "Value Count Mismatch"
          Right (Left l)
            | getValueCount l == cnt -> Right (accum, Just (key, l))
            | otherwise              -> Left "Value Count Mismatch"

  readChildren accum ((key,(cnt,oid)):rest) =
    case Map.lookup oid loaded of
      Nothing -> Left $ cannotFind oid
      Just frag ->
        case fromFragments loaded frag of
          Left err -> Left err
          Right (Right c)
            | getValueCount c == cnt ->
                readChildren (Map.insert key c accum) rest
            | otherwise -> Left "Value Count Mismatch"
          _ -> Left "Got incomplete branch in non-right position"


  cannotFind oid =
    Text.append "Failed to find object with ObjectID "
                (Text.pack $ show oid)

