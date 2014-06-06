-- |
-- Module    : Data.StableTree.IO.Ram
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- A sample implementation of StableTree storage that just writes stuff to some
-- Maps that are wrapped in IORefs.
module Data.StableTree.IO.Ram
( RamError(..)
, storage
) where

import Data.StableTree.IO ( Id, Error(..), Store(..) )

import qualified Data.Map as Map
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import Data.Map   ( Map )
import Data.Text  ( Text )

-- |Error type for RAM storage. Not a lot can go wrong in RAM...
data RamError = NoKey
              | ApiError Text
              deriving ( Show )

instance Error RamError where
  stableTreeError = ApiError

-- |Create a new RAM store
storage :: IO ( Store RamError k v
              , IORef (Map Id (Int,Map k Id))
              , IORef (Map Id v) )
storage = do
  trees  <- newIORef Map.empty
  values <- newIORef Map.empty
  return ( Store (lt trees) (lv values) (st trees) (sv values)
         , trees
         , values )
  where
  lt store tid = do
    m <- readIORef store
    case Map.lookup tid m of
      Nothing -> return $ Left NoKey
      Just pair -> return $ Right pair

  lv store vid = do
    m <- readIORef store
    case Map.lookup vid m of
      Nothing -> return $ Left NoKey
      Just v -> return $ Right v

  st store tid depth tree = do
    modifyIORef store $ Map.insert tid (depth,tree)
    return Nothing

  sv store vid val = do
    modifyIORef store $ Map.insert vid val
    return Nothing

