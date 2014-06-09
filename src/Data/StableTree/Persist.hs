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
-- a user can also implement 'Build' for custom data types.
module Data.StableTree.Persist
( Store(..)
, Build(..)
, Error(..)
, Id
, encodeId
, decodeId
, load
, store
, buildBinary
, buildSerialize
) where

import Data.StableTree.Types hiding ( hash )
import Data.StableTree ( StableTree(..) )

import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Map as Map
import qualified Data.Serialize as S
import Blaze.ByteString.Builder            ( Builder, toByteString )
import Blaze.ByteString.Builder.ByteString ( fromByteString, fromLazyByteString  )
import Blaze.ByteString.Builder.Char8      ( fromShow, fromString, fromChar )
import Blaze.ByteString.Builder.Word       ( fromWord64be )
import Control.Arrow                       ( second )
import Control.Monad.Except                ( ExceptT, runExceptT, lift, throwError )
import Crypto.Hash.Skein256                ( hash )
import Data.ByteString                     ( ByteString )
import Data.Int                            ( Int8, Int16, Int32, Int64 )
import Data.Map                            ( Map )
import Data.Monoid                         ( (<>), mconcat )
import Data.Serialize.Get                  ( runGet, getWord64be )
import Data.Text                           ( Text )
import Data.Word                           ( Word, Word8, Word16, Word32, Word64 )

-- |Things go wrong with end-user storage, but things can also go wrong with
-- reconstructing tree values. Implement 'stableTreeError' to allow 'load' and
-- 'store' to report their own errors.
class Error e where
  stableTreeError :: Text -> e

-- |The opaque type to identify values and branches of trees.
data Id = Id !Word64 !Word64 !Word64 !Word64 deriving ( Show, Eq, Ord )

-- |Convert an Id into a 32-byte ByteString. Useful for actual storage of Ids
encodeId :: Id -> ByteString
encodeId = toByteString . build

-- |Convert a stored ByteString back into an Id. Gives a "Left err" if the
-- given ByteString isn't 32 bytes long
decodeId :: ByteString -> Either String Id
decodeId = runGet decode
  where
  decode = do
    a <- getWord64be
    b <- getWord64be
    c <- getWord64be
    d <- getWord64be
    return $ Id a b c d

-- |Write appropriate functions here to load and store primitive parts of
-- trees.
data Store m e k v = Store
  { loadTree   :: Id -> m (Either e (Depth, ValueCount, Map k Id))
  , loadValue  :: Id -> m (Either e v)
  , storeTree  :: Id -> Depth -> ValueCount -> Map k Id -> m (Maybe e)
  , storeValue :: Id -> v -> m (Maybe e)
  }

-- |Retrieve a tree given its id.
load :: (Monad m, IsKey k, Ord k, Error e)
     => Store m e k v
     -> Id
     -> m (Either e (StableTree k v))
load s i = runExceptT $ load' s i

load' :: (Monad m, IsKey k, Ord k, Error e)
      => Store m e k v
      -> Id
      -> ExceptT e m (StableTree k v)
load' storage treeId =
  liftEither (loadTree storage treeId) >>= \case
    (0, _, contents)     -> loadBottom contents
    (depth, _, contents) -> loadBranch depth contents
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
      Just ((k,valId),rest) -> do
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
      Just ((k,valId),rest) -> do
        subtree <- load' storage valId
        loadChildren rest $ Map.insert k subtree accum

  err = throwError . stableTreeError

-- |Store a tree using a 'Store' and return its calculated 'Id'
store :: (Monad m, Build k, Ord k, Build v)
      => Store m e k v
      -> StableTree k v
      -> m (Either e Id)
store storage (StableTree_I i) = runExceptT $ store' storage i
store storage (StableTree_C c) = runExceptT $ store' storage c

store' :: (Monad m, Build k, Ord k, Build v)
       => Store m e k v
       -> Tree c k v
       -> ExceptT e m Id
store' storage tree =
  case branchContents tree of
    Left subtrees -> storeBranch subtrees
    Right kvmap -> storeBottom kvmap

  where
  storeBranch (complete, mIncomplete) = do
    key_ids <- storeSubtrees complete Map.empty
    case mIncomplete of
      Nothing -> storeKeyIds key_ids
      Just (k,v) -> do
        treeId <- store' storage v
        storeKeyIds $ Map.insert k treeId key_ids

  storeSubtrees kvmap accum =
    case Map.minViewWithKey kvmap of
      Nothing -> return accum
      Just ((k,t), rest) -> do
        treeId <- store' storage t
        storeSubtrees rest $ Map.insert k treeId accum

  storeBottom kvmap = do
    key_ids <- storeValues kvmap Map.empty
    storeKeyIds key_ids

  storeValues kvmap accum = do
    case Map.minViewWithKey kvmap of
      Nothing -> return accum
      Just ((k,v), rest) -> do
        let valId = calcId $ build v
        _ <- liftMaybe $ storeValue storage valId v
        storeValues rest $ Map.insert k valId accum

  storeKeyIds key_ids =
    let depth = getDepth tree
        vcount = getValueCount tree
        valId = treeHash depth key_ids
    in do liftMaybe $ storeTree storage valId depth vcount key_ids
          return valId

treeHash :: Build k => Int -> Map k Id -> Id
treeHash depth contents =
  let builders = [(build k, build v) | (k,v) <- Map.toAscList contents]
      w_len    = [(len k, k, v) | (k, v) <- builders]
      bodybs   = toByteString $ mconcat [l <> k <> v | (l,k,v) <- w_len]
      bodylen  = fromShow $ BS.length bodybs
      header   = (fromString "tree ")
                 <> bodylen <> fromChar ' '
                 <> fromShow depth <> fromChar '\0'
  in calcId $ header <> fromByteString bodybs
  where
  len = fromShow . BS.length . toByteString

calcId :: Builder -> Id
calcId = right . decodeId . hash 256 . toByteString
  where
  right ei =
    case ei of
      Left _ -> error "Got a left!?"
      Right v -> v

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

-- |Typeclass to generate unique 'ByteString's for StableTree keys and values.
-- Used to generate the unique identities for values and branches.
class Build t where
  build :: t -> Builder

-- |Generate a builder for something that is already a 'Binary'
buildBinary :: B.Binary t => t -> Builder
buildBinary = fromLazyByteString . B.encode

-- |Generate a builder for something that is already a 'Serialize'
buildSerialize :: S.Serialize t => t -> Builder
buildSerialize = fromByteString . S.encode

instance Build Id where
  build (Id a b c d) = w a <> w b <> w c <> w d
    where
    w = fromWord64be

instance Build Char where
  build = buildSerialize

instance Build Double where
  build = buildSerialize

instance Build Float where
  build = buildSerialize

instance Build Int where
  build = buildSerialize

instance Build Int8 where
  build = buildSerialize

instance Build Int16 where
  build = buildSerialize

instance Build Int32 where
  build = buildSerialize

instance Build Int64 where
  build = buildSerialize

instance Build Integer where
  build = buildSerialize

instance Build Word where
  build = buildSerialize

instance Build Word8 where
  build = buildSerialize

instance Build Word16 where
  build = buildSerialize

instance Build Word32 where
  build = buildSerialize

instance Build Word64 where
  build = buildSerialize

instance Build ByteString where
  build = fromByteString

instance Build Lazy.ByteString where
  build = fromLazyByteString

