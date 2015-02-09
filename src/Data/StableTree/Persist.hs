{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- |
-- Module    : Data.StableTree.Persist
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- Logic for dealing with the actual persistence of Stable Trees. The key
-- exports here are 'Error', 'load', and 'store'. A user needs to make an
-- appropriate Error type to report storage errors, and then the 'load' and
-- 'store' functions can just do their thing. If necessary, a user can also
-- implement 'Serialize' for custom data types.
module Data.StableTree.Persist
( Error(..)
, Fragment(..)
, store
, store'
, load
, load'
) where

import Data.StableTree.Conversion ( Fragment(..), toFragments, fromFragments )
import Data.StableTree.Key        ( StableKey )
import Data.StableTree.Types      ( StableTree(..) )

import qualified Data.Map as Map
import Data.ObjectID  ( ObjectID )
import Data.Serialize ( Serialize )
import Data.Text      ( Text )

-- |Things go wrong with end-user storage, but things can also go wrong with
-- reconstructing tree values. Implement 'stableTreeError' to allow 'load' and
-- 'store' to report their own errors.
class Error e where
  stableTreeError :: Text -> e

-- |Record the tree into storage. This works like a fold, where the function
-- takes an accumulating state and each tree fragment to store, while returning
-- either an error message (which will abort the loop immediately) or the next
-- state for the accumulator.
--
-- Any fragment referring to other fragments ('FragmentBranch' fragments) will
-- be given to the fold only after all their children have been given to the
-- fold. Exact ordering beyond that is not guaranteed, but the current
-- behaviour is post-order depth-first traversal.
store :: (Monad m, Error e, Ord k, Serialize k, StableKey k, Serialize v)
      => (a -> ObjectID -> Fragment k v -> m (Either e a))
      -> a
      -> StableTree k v
      -> m (Either e a)
store fn a0 = go a0 . toFragments
  where
  go accum [] = return $ Right accum
  go accum (frag:frags) =
    fn accum (fragmentObjectID frag) frag >>= \case
      Left err -> return $ Left err
      Right accum' -> go accum' frags

-- |Alternate store function that acts more like a map than a fold. See 'store'
-- for details.
store' :: (Monad m, Error e, Ord k, Serialize k, StableKey k, Serialize v)
       => (ObjectID -> Fragment k v -> m (Maybe e))
       -> StableTree k v
       -> m (Either e ObjectID)
store' fn = store fn' undefined
  where
  fn' _accum oid frag =
    fn oid frag >>= \case
      Nothing -> return $ Right oid
      Just err -> return $ Left err

-- |Reverse of 'store'. As with 'store', this acts like a fold, but converts an
-- 'ObjectID' into a tree, rather than storing a tree. This will always build
-- the tree from the top down.
load :: (Monad m, Error e, Ord k, Serialize k, StableKey k, Serialize v)
     => (a -> ObjectID -> m (Either e (a, Fragment k v)))
     -> a
     -> ObjectID
     -> m (Either e (a, StableTree k v))
load fn a0 top =
  recur a0 Map.empty [top] >>= \case
    Left err ->
      return $ Left err
    Right (accum, frags) ->
      case Map.lookup top frags of
        Nothing ->
          return $ Left (stableTreeError "load/recur failed to find top")
        Just frag ->
          case fromFragments frags frag of
            Left err -> return $ Left (stableTreeError err)
            Right t -> return $ Right (accum, t)

  where
  recur accum frags [] = return $ Right (accum, frags)
  recur accum frags (oid:rest) = fn accum oid >>= \case
    Left err -> return $ Left err
    Right (accum', frag@(FragmentBottom{})) ->
      recur accum' (Map.insert oid frag frags) rest
    Right (accum', frag) ->
      let children = fragmentChildren frag
          oids     = map snd $ Map.elems children
      in recur accum' (Map.insert oid frag frags) (oids ++ rest)

-- |Version of 'load' that acts like a map rather than a fold.
load' :: (Monad m, Error e, Ord k, Serialize k, StableKey k, Serialize v)
      => (ObjectID -> m (Either e (Fragment k v)))
      -> ObjectID
      -> m (Either e (StableTree k v))
load' fn top =
  load fn' undefined top >>= \case
    Left err -> return $ Left err
    Right (_, tree) -> return $ Right tree
  where
  fn' st oid =
    fn oid >>= \case
      Left err -> return $ Left err
      Right frag -> return $ Right (st, frag)

