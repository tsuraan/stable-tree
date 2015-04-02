{-# LANGUAGE GADTs, LambdaCase, OverloadedStrings #-}
-- |
-- Module    : Data.StableTree.Store
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- Logic for dealing with the actual persistence of Stable Trees. The key
-- exports here are 'StoreError', 'load', and 'store'. A user needs to make an
-- appropriate StoreError instance of their load/store monad to report storage
-- errors, and then the 'load' and 'store' functions can just do their thing.
-- If necessary, a user can also implement 'Serialize' for custom data types.
module Data.StableTree.Store
( Fragment(..)
, StoreError(..)
, store
, store'
, load
, load'
) where

import Data.StableTree.Types
import Data.StableTree.Key        ( StableKey )
import Data.StableTree.Properties ( bottomChildren, branchChildren )
import Data.StableTree.Build      ( consume, consumeMap )

import qualified Data.Map as Map
import Control.Applicative    ( Applicative, (<$>) )
import Control.Monad          ( replicateM )
import Control.Monad.Identity ( Identity)
import Data.Map               ( Map )
import Data.Serialize         ( Serialize, get, put )
import Data.Serialize.Get     ( Get, getByteString )
import Data.Serialize.Put     ( Put, putByteString )

-- |A 'Fragment' is a user-visible part of a tree, i.e. a single node in the
-- tree that can actually be manipulated by a user. This is useful when doing
-- the work of persisting trees. See `Data.StableTree.Store.store` and
-- `Data.StableTree.Store.load` for functions related to storing and retrieving
-- Fragments.
data Fragment t k v
  = FragmentBranch
    { fragmentDepth    :: Depth
    , fragmentChildren :: Map k (ValueCount, t)
    }
  | FragmentBottom
    { fragmentMap :: Map k v
    }

-- |'load'/'store' and the functions passed to them run in a 'StoreError'
-- Monad, to allow the internal StableTree mechanisms to report errors in
-- serializing and rebuilding tree elements.
--
-- Right now, the functions could just as well be the same as 'fail' (and are,
-- for the Identity and IO implementations of StoreError), but they might grow
-- into something more useful in the future.
class (Applicative m, Monad m) => StoreError m where
  serializeError      :: String -> m a
  reconstitutionError :: String -> m a

-- |Record the tree into storage. This works like a fold, where the function
-- takes an accumulating state and each tree fragment to store, while returning
-- the next state for the accumulator.
--
-- Any fragment referring to other fragments ('FragmentBranch' fragments) will
-- be given to the fold only after all their children have been given to the
-- fold. Exact ordering beyond that is not guaranteed, but the current
-- behaviour is post-order depth-first traversal.
store :: (StoreError m, Ord k)
      => (a -> Fragment t k v -> m (t, a))
      -> a
      -> StableTree k v
      -> m (t, a)
store fn a0 tree =
  case tree of
    StableTree_I i -> storeTree fn a0 i
    StableTree_C c -> storeTree fn a0 c

-- |Alternate store function that acts more like a map than a fold. See 'store'
-- for details.
store' :: (StoreError m, Ord k)
       => (Fragment t k v -> m t)
       -> StableTree k v
       -> m t
store' fn tree = fst <$> store fn' a0 tree
  where
  fn' accum frag = do
    t <- fn frag
    return (t, accum)

  a0 = undefined

storeTree :: (StoreError m, Ord k)
          => (a -> Fragment t k v -> m (t, a))
          -> a
          -> Tree d c k v
          -> m (t, a)
storeTree fn a0 b@(Bottom{})   = storeBottom fn a0 b
storeTree fn a0 b@(IBottom0{}) = storeBottom fn a0 b
storeTree fn a0 b@(IBottom1{}) = storeBottom fn a0 b
storeTree fn a0 b@(Branch{})   = storeBranch fn a0 b
storeTree fn a0 b@(IBranch0{}) = storeBranch fn a0 b
storeTree fn a0 b@(IBranch1{}) = storeBranch fn a0 b
storeTree fn a0 b@(IBranch2{}) = storeBranch fn a0 b

storeBottom :: (StoreError m, Ord k)
            => (a -> Fragment t k v -> m (t, a))
            -> a
            -> Tree Z c k v
            -> m (t, a)
storeBottom fn a0 bottom =
  let children = bottomChildren bottom
      frag     = FragmentBottom children
  in fn a0 frag

storeBranch :: (StoreError m, Ord k)
            => (a -> Fragment t k v -> m (t, a))
            -> a
            -> Tree (S d) c k v
            -> m (t, a)
storeBranch fn a0 branch =
  let (comps, incomp) = branchChildren branch
      comps'          = [ (k, (vc, StableTree_C t))
                        | (k, (vc, t)) <- Map.toList comps]
      incomp'         = case incomp of
                          Nothing -> []
                          Just (k, vc, t) -> [(k, (vc, StableTree_I t))]
  in storeChildren fn a0 (getDepth branch) Map.empty (comps' ++ incomp')

storeChildren :: (StoreError m, Ord k)
              => (a -> Fragment t k v -> m (t, a))
              -> a
              -> Depth
              -> Map k (ValueCount, t)
              -> [(k, (ValueCount, StableTree k v))]
              -> m (t, a)
storeChildren _ _ _ _ [] =
  serializeError "Branch must have children!"
storeChildren fn a0 d ch [(k, (vc, t))] = do
  (tag, accum) <- store fn a0 t
  fn accum $ FragmentBranch d (Map.insert k (vc, tag) ch)
storeChildren fn a0 d ch ((k, (vc, t)):rest) = do
  (tag, accum) <- store fn a0 t
  storeChildren fn accum d (Map.insert k (vc, tag) ch) rest

-- |Reverse of 'store'. As with 'store', this acts like a fold, but converts an
-- tag into a tree, rather than storing a tree. This will always build the tree
-- from the top down.
load :: (StoreError m, Ord k, StableKey k)
     => (a -> t -> m (Fragment t k v, a))
     -> a
     -> t
     -> m (StableTree k v, a)
load fn a0 tag = do
  (frag, a1)    <- fn a0 tag
  (bottoms, a2) <- growBottoms [] fn a1 frag
  return (uncurry consume bottoms, a2)

-- |Version of 'load' that acts like a map rather than a fold.
load' :: (StoreError m, Ord k, StableKey k)
      => (t -> m (Fragment t k v))
      -> t
      -> m (StableTree k v)
load' fn tag = fst <$> load fn' a0 tag
  where
  fn' accum t = do
    frag <- fn t
    return (frag, accum)

  a0 = undefined

growBottoms :: (StoreError m, Ord k, StableKey k)
            => [Tree Z Complete k v]
            -> (a -> t -> m (Fragment t k v, a))
            -> a
            -> Fragment t k v
            -> m (([Tree Z Complete k v], Maybe (Tree Z Incomplete k v)), a)
growBottoms bottoms _fn a (FragmentBottom m) =
  makeBottom bottoms m a
growBottoms bottoms fn a (FragmentBranch _ ch) =
  let tags = map snd $ Map.elems ch
  in loadBranch bottoms fn a tags

loadBranch :: (StoreError m, Ord k, StableKey k)
           => [Tree Z Complete k v]
           -> (a -> t -> m (Fragment t k v, a))
           -> a
           -> [t]
           -> m (([Tree Z Complete k v], Maybe (Tree Z Incomplete k v)), a)
loadBranch bottoms _ a0 [] = return ((bottoms, Nothing), a0)
loadBranch bottoms fn a0 (t:ts) = do
  (frag, a1)     <- fn a0 t
  (bpair, a2) <- growBottoms bottoms fn a1 frag
  case bpair of
    (bottoms', Nothing) ->
      loadBranch bottoms' fn a2 ts
    final ->
      -- It has an Incomplete element, so we cannot add anything else to it.
      -- We're done.
      return (final, a2)

makeBottom :: (StoreError m, Ord k, StableKey k)
           => [Tree Z Complete k v]
           -> Map k v
           -> a
           -> m (([Tree Z Complete k v], Maybe (Tree Z Incomplete k v)), a)
makeBottom bottoms m a =
  case consumeMap m of
    ([], Just bottom) ->
      return ((bottoms, Just bottom), a)
    ([bottom], Nothing) ->
      return ((bottoms ++ [bottom], Nothing), a)
    _ ->
      reconstitutionError "Rebuild fragment became multiple trees"

instance (Ord k, Serialize k, Serialize v, Serialize t)
      => Serialize (Fragment t k v) where
  put frag =
    case frag of
      (FragmentBranch depth children) -> fragPut depth children
      (FragmentBottom values)         -> fragPut 0 values
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
            return $ FragmentBottom children
          depth -> do
            count <- get
            children <- Map.fromList <$> replicateM count getPair
            return $ FragmentBranch depth children
      _ -> fail "Not a serialized Fragment"
    where
    getPair :: (Serialize k, Serialize v) => Get (k,v)
    getPair = do
      k <- get
      v <- get
      return (k,v)

instance StoreError IO where
  serializeError      = fail
  reconstitutionError = fail

instance StoreError Identity where
  serializeError      = fail
  reconstitutionError = fail

