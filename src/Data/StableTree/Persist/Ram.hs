{-# LANGUAGE LambdaCase #-}
-- |
-- Module    : Data.StableTree.Persist.Ram
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- A sample implementation of StableTree storage that just writes stuff to some
-- Maps that are wrapped in IORefs.
module Data.StableTree.Persist.Ram
( RamError(..)
, store
, load
) where

import Data.StableTree.Persist ( Error(..) )
import Data.StableTree.Fragment ( Fragment )

import qualified Data.Map as Map
import Control.Monad.State.Strict ( State, modify, gets )
import Data.Map   ( Map )
import Data.ObjectID ( ObjectID )
import Data.Text  ( Text )
import Data.Serialize ( Serialize, encode, decode )
import Data.ByteString ( ByteString )

-- |Error type for RAM storage. Not a lot can go wrong in RAM...
data RamError = NotFound ObjectID
              | SerializationError String
              | ApiError Text
              deriving ( Show )

instance Error RamError where
  stableTreeError = ApiError

type StableTreeState = State (Map ByteString ByteString)

store :: (Ord k, Serialize k, Serialize v)
      => ObjectID -> Fragment k v -> StableTreeState (Maybe RamError)
store oid frag = do
  modify $ Map.insert (encode oid) (encode frag)
  return Nothing

load :: (Ord k, Serialize k, Serialize v)
     => ObjectID -> StableTreeState (Either RamError (Fragment k v))
load oid =
  gets (Map.lookup $ encode oid) >>= \case
    Nothing -> return $ Left $ NotFound oid
    Just fragBS ->
      case decode fragBS of
        Left err -> return $ Left $ SerializationError err
        Right frag -> return $ Right frag

