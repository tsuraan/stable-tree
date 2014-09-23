{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- |
-- Module    : Data.StableTree.Persist
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- Logic for dealing with the actual persistence of Stable Trees. The key
-- exports here are 'Error', 'Store', 'load', and 'store'. A user needs to
-- implement the 'loadTree', 'loadValue', 'storeTree' and 'storeValue' parts of
-- 'Store', and make an appropriate Error type to report storage errors, and
-- then the 'load' and 'store' functions can just do their thing. If necessary,
-- a user can also implement 'Serialize' for custom data types.
module Data.StableTree.Persist
( Store(..)
, Error(..)
, load
, store
) where

import Data.StableTree.Types hiding ( hash )
import Data.StableTree ( StableTree(..) )

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.ObjectID as ObjectID
import Blaze.ByteString.Builder            ( toByteString )
import Blaze.ByteString.Builder.Char8      ( fromShow, fromString, fromChar )
import Control.Arrow                       ( second )
import Control.Monad.Except                ( ExceptT, runExceptT, lift, throwError )
import Data.Map                            ( Map )
import Data.Monoid                         ( (<>) )
import Data.ObjectID                       ( ObjectID )
import Data.Serialize                      ( Serialize(..) )
import Data.Serialize.Put                  ( runPut )
import Data.Text                           ( Text )

-- |Things go wrong with end-user storage, but things can also go wrong with
-- reconstructing tree values. Implement 'stableTreeError' to allow 'load' and
-- 'store' to report their own errors.
class Error e where
  stableTreeError :: Text -> e

-- |Write appropriate functions here to load and store primitive parts of
-- trees.
data Store m e k v = Store
  { loadTree   :: ObjectID -> m (Either e (Depth, Map k (ValueCount, ObjectID)))
  , loadValue  :: ObjectID -> m (Either e v)
  , storeTree  :: ObjectID -> Depth -> Map k (ValueCount, ObjectID) -> m (Maybe e)
  , storeValue :: ObjectID -> v -> m (Maybe e)
  }

-- |Retrieve a tree given its id.
load :: (Monad m, IsKey k, Ord k, Error e)
     => Store m e k v
     -> ObjectID
     -> m (Either e (StableTree k v))
load s i = runExceptT $ load' s i

load' :: (Monad m, IsKey k, Ord k, Error e)
      => Store m e k v
      -> ObjectID
      -> ExceptT e m (StableTree k v)
load' storage treeId =
  liftEither (loadTree storage treeId) >>= \case
    (0, contents)     -> loadBottom contents
    (depth, contents) -> loadBranch depth contents
  where
  loadBottom contents = do
    vals <- loadValues contents Map.empty
    case nextBottom vals of
      Left i -> return $ StableTree_I i
      Right (c,r) ->
        if Map.null r
          then return $ StableTree_C c
          else err "Too many terminal keys in loadBottom"

  loadValues cont accum =
    case Map.minViewWithKey cont of
      Nothing -> return accum
      Just ((k,(_,valId)),rest) -> do
        v <- liftEither $ loadValue storage valId
        loadValues rest $ Map.insert k v accum

  loadBranch depth contents = do
    children <- loadChildren contents Map.empty
    let classify s = case s of StableTree_I i -> Right i
                               StableTree_C c -> Left c
        (cs, is)   = Map.mapEither classify children
    case Map.minViewWithKey is >>= return . second Map.minViewWithKey of
      Nothing -> go cs Nothing
      Just (_, Just (_,_)) ->
        err "Too many incomplete trees in loadBranch"
      Just ((ik,iv), Nothing) ->
        case Map.maxViewWithKey cs of
          Nothing -> go cs $ Just (ik, iv)
          Just ((ck,_), _) ->
            if ck > ik
              then err "Saw complete trees after incomplete..."
              else go cs $ Just (ik, iv)
    where
    go completes mIncomplete =
      case nextBranch completes mIncomplete of
        Left i ->
          if getDepth i == depth
            then return $ StableTree_I i
            else err "Depth mismatch in loadBranch"
        Right (c,m) | Map.null m ->
          if getDepth c == depth
            then return $ StableTree_C c
            else err "Depth mismatch in loadBranch"
        _ -> err "Too many terminal keys in loadBranch"

  loadChildren cont accum =
    case Map.minViewWithKey cont of
      Nothing -> return accum
      Just ((k,(_,valId)),rest) -> do
        subtree <- load' storage valId
        loadChildren rest $ Map.insert k subtree accum

  err = throwError . stableTreeError

-- |Store a tree using a 'Store' and return its calculated 'ObjectID'
store :: (Monad m, Serialize k, Ord k, Serialize v)
      => Store m e k v
      -> StableTree k v
      -> m (Either e ObjectID)
store storage (StableTree_I i) = runExceptT $ store' storage i
store storage (StableTree_C c) = runExceptT $ store' storage c

store' :: (Monad m, Serialize k, Ord k, Serialize v)
       => Store m e k v
       -> Tree c k v
       -> ExceptT e m ObjectID
store' storage tree =
  case branchContents tree of
    Left subtrees -> storeBranch subtrees
    Right kvmap -> storeBottom kvmap

  where
  storeBranch (complete, mIncomplete) = do
    key_ids <- storeSubtrees complete Map.empty
    case mIncomplete of
      Nothing -> storeKeyIds key_ids
      Just (k,c,v) -> do
        treeId <- store' storage v
        storeKeyIds $ Map.insert k (c,treeId) key_ids

  storeSubtrees kvmap accum =
    case Map.minViewWithKey kvmap of
      Nothing -> return accum
      Just ((k,(c,t)), rest) -> do
        treeId <- store' storage t
        storeSubtrees rest $ Map.insert k (c,treeId) accum

  storeBottom kvmap = do
    key_ids <- storeValues kvmap Map.empty
    storeKeyIds key_ids

  storeValues kvmap accum = do
    case Map.minViewWithKey kvmap of
      Nothing -> return accum
      Just ((k,v), rest) -> do
        let valId = ObjectID.calculateSerialize v
        _ <- liftMaybe $ storeValue storage valId v
        storeValues rest $ Map.insert k (1,valId) accum

  storeKeyIds key_ids =
    let depth = getDepth tree
        valId = treeHash depth $ Map.map snd key_ids
    in do liftMaybe $ storeTree storage valId depth key_ids
          return valId

treeHash :: Serialize k => Int -> Map k ObjectID -> ObjectID
treeHash depth contents =
  let bodybs   = runPut putBody
      bodylen  = fromShow $ BS.length bodybs
      header   = toByteString $ (fromString "tree ")
                                <> bodylen <> fromChar ' '
                                <> fromShow depth <> fromChar '\0'
  in ObjectID.calculateByteString $ BS.append header bodybs
  where
  putBody = mapM_ (\(k,v) -> put k >> put v) (Map.toAscList contents)

liftEither :: Monad m => m (Either a b) -> ExceptT a m b
liftEither act =
  lift act >>= \case
    Left err -> throwError err
    Right val -> return val

liftMaybe :: Monad m => m (Maybe e) -> ExceptT e m ()
liftMaybe act =
  lift act >>= \case
    Just err -> throwError err
    Nothing -> return ()

