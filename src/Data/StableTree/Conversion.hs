{-# LANGUAGE OverloadedStrings, LambdaCase, GADTs #-}
-- |
-- Module    : Data.StableTree.Conversion
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- Functions for converting between `Tree` and `Fragment` types
module Data.StableTree.Conversion
( Fragment(..)
, toFragments
, fromFragments
, fragsToMap
) where

import Data.StableTree.Properties ( bottomChildren, branchChildren )
import Data.StableTree.Build      ( consume, consumeMap )
import Data.StableTree.Key        ( StableKey )
import Data.StableTree.Types

import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Applicative ( (<$>) )
import Control.Monad       ( replicateM )
import Data.Map            ( Map )
import Data.ObjectID       ( ObjectID, calculateSerialize )
import Data.Serialize      ( Serialize, get, put )
import Data.Serialize.Get  ( Get, getByteString )
import Data.Serialize.Put  ( Put, putByteString )
import Data.Text           ( Text )

-- |A 'Fragment' is a user-visible part of a tree, i.e. a single node in the
-- tree that can actually be manipulated by a user. This is useful when doing
-- the work of persisting trees. See `Data.StableTree.Conversion.toFragments`
-- and `Data.StableTree.Conversion.fromFragments` for functions to convert
-- between Fragments and Trees. see `Data.StableTree.Persist.store` and
-- `Data.StableTree.Persist.load` for functions related to storing and
-- retrieving Fragments.
data Fragment k v
  = FragmentBranch
    { fragmentObjectID :: ObjectID
    , fragmentDepth    :: Depth
    , fragmentChildren :: Map k (ValueCount, ObjectID)
    }
  | FragmentBottom
    { fragmentObjectID :: ObjectID
    , fragmentMap      :: Map k v
    }
  deriving( Eq, Ord, Show )

-- |Convert a 'StableTree' 'Tree' into a list of storable 'Fragment's. The
-- resulting list is guaranteed to be in an order where each 'Fragment' will be
-- seen after all its children.
toFragments :: (Ord k, Serialize k, StableKey k, Serialize v)
            => StableTree k v -> [Fragment k v]
toFragments (StableTree_I i) = snd $ toFragments' i
toFragments (StableTree_C c) = snd $ toFragments' c

-- |Convert a 'Tree' into 'Fragment's. This returns a pair, where the first
-- element is the 'Fragment' that came directly from the 'Tree', and the second
-- element is the list of all the 'Fragment's beneath the 'Tree', and the
-- 'Tree's fragment itself. The list is always sorted lowest to highest, so its
-- last element is always the same entity as the first element of the pair. 
toFragments' :: (Ord k, Serialize k, StableKey k, Serialize v)
             => Tree d c k v -> (Fragment k v, [Fragment k v])
toFragments' b@(Bottom{})   = bottomToFragments b
toFragments' b@(IBottom0{}) = bottomToFragments b
toFragments' b@(IBottom1{}) = bottomToFragments b
toFragments' b@(Branch{})   = branchToFragments b
toFragments' b@(IBranch0{}) = branchToFragments b
toFragments' b@(IBranch1{}) = branchToFragments b
toFragments' b@(IBranch2{}) = branchToFragments b

-- |Make a Bottom element into a FragmentBottom. Always returns
-- (fragment, [fragment])
bottomToFragments :: (Ord k, Serialize k, StableKey k, Serialize v)
                  => Tree Z c k v -> (Fragment k v, [Fragment k v])
bottomToFragments tree =
  let children = bottomChildren tree
      frag     = fixFragmentID $ FragmentBottom undefined children
  in (frag, [frag])

-- |Make a Branch into a bunch of Fragments
branchToFragments :: (Ord k, Serialize k, StableKey k, Serialize v)
                  => Tree (S d) c k v -> (Fragment k v, [Fragment k v])
branchToFragments tree =
  let (completes, mInc) = branchChildren tree
      compFrags         = Map.map (\(v, t) -> (v, toFragments' t)) completes
      allFrags          = case mInc of
                            Nothing ->
                              compFrags
                            Just (k, v, t) ->
                              Map.insert k (v, toFragments' t) compFrags
      getChildPair     = \(v, (f, _)) -> (v, fragmentObjectID f)
      children         = Map.map getChildPair allFrags
      cumulative       = concat $ map (snd . snd) $ Map.elems allFrags
      depth            = getDepth tree
      frag             = fixFragmentID $ FragmentBranch undefined depth children
  in (frag, cumulative ++ [frag])

-- |Recover a 'Tree' from a single 'Fragment' and a map of the fragments as
-- returned from 'toFragments'. If the fragment set was already stored, it is
-- the caller's responsibility to load all the child fragments into a map
-- (probably involving finding children using the fragmentChildren field of the
-- Fragment type).
fromFragments :: (Ord k, Serialize k, StableKey k, Serialize v)
              => Map ObjectID (Fragment k v)
              -> Fragment k v
              -> Either Text (StableTree k v)
fromFragments loaded top = do
  (complete, mincomplete) <- fragsToBottoms loaded top
  return $ consume complete mincomplete

-- |Directly convert a bunch of `Fragment`s and a root fragment into a
-- `Data.Map.Map` instance. Mostly useful for testing the correctness of the
-- `fromFragments` function.
fragsToMap :: Ord k
           => Map ObjectID (Fragment k v)
           -> Fragment k v
           -> Either Text (Map k v)
fragsToMap loaded = go Map.empty
  where
  go accum (FragmentBottom _ m) = Right $ Map.union accum m
  go accum (FragmentBranch _ _ children) =
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

-- |Build a list of the 'Tree Z' instances that come from the given 'Fragment'.
-- The resulting Trees non-overlapping and ordered such that each Tree's
-- highest key is lower than the next Tree's lowest key, but illegal Fragments
-- could break that.
fragsToBottoms :: (Ord k, Serialize k, StableKey k, Serialize v)
               => Map ObjectID (Fragment k v)
               -> Fragment k v
               -> Either Text ( [Tree Z Complete k v]
                              , Maybe (Tree Z Incomplete k v))
fragsToBottoms _ (FragmentBottom _ m) = Right $ consumeMap m
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

instance (Ord k, Serialize k, Serialize v) => Serialize (Fragment k v) where
  put frag =
    case frag of
      (FragmentBranch _ depth children) -> fragPut depth children
      (FragmentBottom _ values)         -> fragPut 0 values
    where
    fragPut :: (Serialize k, Serialize v) => Depth -> Map k v -> Put
    fragPut depth items = do
      putByteString "stable-tree\0"
      put depth
      put $ Map.size items
      mapM_ (\(k,v) -> put k >> put v) (Map.toAscList items)

  get =
    getByteString 12 >>= \case
      "stable-tree\0" ->
        get >>= \case
          0 -> do
            count <- get
            children <- Map.fromList <$> replicateM count getPair
            -- Having to create a broken fragment, serialize it, and then
            -- calculate that bytestring's ObjectID is gross, when we already
            -- have the serialized form of the fragment, but I have no idea how
            -- to access the underlying bytestring. This should be correct, but
            -- it's not very efficient.
            return $ fixFragmentID $ FragmentBottom undefined children
          depth -> do
            count <- get
            children <- Map.fromList <$> replicateM count getPair
            return $ fixFragmentID $ FragmentBranch undefined depth children
      _ -> fail "Not a serialized Fragment"
    where
    getPair :: (Serialize k, Serialize v) => Get (k,v)
    getPair = do
      k <- get
      v <- get
      return (k,v)

-- |Calculate the 'Fragment's 'ObjectID', and patch it into place. This is
-- generally used to create a 'Fragment', like so:
--
-- @
-- fixFragmentID $ FragmentBottom undefined foo
-- fixFragmentID $ FragmentBranch undefined foo bar
-- @
fixFragmentID :: (Ord k, Serialize k, Serialize v)
              => Fragment k v -> Fragment k v
fixFragmentID frag@(FragmentBottom _ children) =
  FragmentBottom (calculateSerialize frag) children
fixFragmentID frag@(FragmentBranch _ depth children) =
  FragmentBranch (calculateSerialize frag) depth children

