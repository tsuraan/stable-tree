{-# LANGUAGE LambdaCase, OverloadedStrings, GADTs #-}
-- |
-- Module    : Data.StableTree.Types
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- This is the implementation of tree fragments, which are stand-alone chunks
-- of data representing each branch within a 'Data.StableTree.Types.Tree'. This
-- module is also used by the Tree code for generating each branch's
-- 'ObjectID'.
module Data.StableTree.Fragment
( Fragment(..)
) where

import Data.StableTree.Types ( Depth, ValueCount )

import qualified Data.Map as Map
import Control.Monad      ( replicateM )
import Data.ObjectID      ( ObjectID )
import Data.Serialize     ( Serialize(..) )
import Control.Applicative ( (<$>) )
import Data.Serialize.Put ( Put, putByteString )
import Data.Serialize.Get ( Get, getByteString )
import Data.Map           ( Map )

-- |A 'Fragment' is a user-visible part of a tree, i.e. a single node in the
-- tree that can actually be manipulated by a user. This is useful when doing
-- the work of persisting trees.
data Fragment k v
  = FragmentBranch
    { fragmentDepth    :: Depth
    , fragmentChildren :: Map k (ValueCount, ObjectID)
    }
  | FragmentBottom
    { fragmentMap :: Map k v
    }
  deriving( Eq, Ord, Show )

instance (Ord k, Serialize k, Serialize v) => Serialize (Fragment k v) where
  put (FragmentBranch depth children) = fragPut depth children
  put (FragmentBottom values)         = fragPut 0 values

  get =
    getByteString 12 >>= \case
      "stable-tree\0" -> do
        get >>= \case
          0 -> do
            count <- get
            children <- Map.fromList <$> replicateM count getPair
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

fragPut :: (Serialize k, Serialize v) => Depth -> Map k v -> Put
fragPut depth items = do
  putByteString "stable-tree\0"
  put depth
  put $ Map.size items
  mapM_ (\(k,v) -> put k >> put v) (Map.toAscList items)

