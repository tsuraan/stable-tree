{-# LANGUAGE LambdaCase #-}
module Main
( main
) where

import Data.StableTree.Fragment ( Fragment )
import Data.StableTree.Persist  ( Error(..) )

import qualified Data.StableTree         as ST
import qualified Data.StableTree.Persist as Persist

import qualified Data.Map as Map
import Control.Arrow              ( first )
import Control.Monad.State.Strict ( State, runState, modify, gets )
import Data.ByteString            ( ByteString )
import Data.ByteString.Arbitrary  ( ArbByteString(..) )
import Data.Map                   ( Map )
import Data.ObjectID              ( ObjectID )
import Data.Serialize             ( Serialize, encode, decode )
import Data.Text                  ( Text )
import Test.Tasty
import Test.Tasty.QuickCheck      ( testProperty )

main :: IO ()
main = defaultMain $
  testGroup "StableTree"
  [ testGroup "Pure"
    [ testProperty "Int/Int" int_int
    , testProperty "Float/Int" float_int
    , testProperty "ByteString/Int" bytestring_int
    ]
  , testGroup "Stored"
    [ testProperty "Int/Int" store_int_int
    , testProperty "Float/Int" store_float_int
    , testProperty "ByteString/Int" store_bytestring_int
    ]
  ]
  where

  int_int :: [(Int,Int)] -> Bool
  int_int pairs =
    let m = Map.fromList pairs
        st = ST.fromMap m
    in m == ST.toMap st

  float_int :: [(Float,Int)] -> Bool
  float_int pairs =
    let m = Map.fromList pairs
        st = ST.fromMap m
    in m == ST.toMap st

  bytestring_int :: [(ArbByteString,Int)] -> Bool
  bytestring_int pairs =
    let m = Map.fromList $ map (first fromABS) pairs
        st = ST.fromMap m
    in m == ST.toMap st

  store_int_int :: [(Int,Int)] -> Bool
  store_int_int = action

  store_float_int :: [(Float,Int)] -> Bool
  store_float_int = action

  store_bytestring_int :: [(ArbByteString,Int)] -> Bool
  store_bytestring_int = action . map (first fromABS)

  action :: (Eq k, Ord k, Serialize k, Eq v, Serialize v) => [(k,v)] -> Bool
  action pairs = fst $ runState go Map.empty
    where
    go = do
      let m  = Map.fromList pairs
          st = ST.fromMap m
      Right tid <- Persist.store' store st
      Right st' <- Persist.load' load tid
      return $ m == ST.toMap st'

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

