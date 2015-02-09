{-# LANGUAGE LambdaCase #-}
module Main
( main
) where

import qualified Data.StableTree as ST
import qualified Data.StableTree.Persist as SP
import Data.StableTree ( StableTree )
import Data.StableTree.Persist ( Fragment(..), Error(..) )
import Data.StableTree.Key     ( StableKey )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow              ( first )
import Control.Applicative        ( (<$>) )
import Control.Monad.State.Strict ( State, runState, modify, gets )
import Data.ByteString            ( ByteString )
import Data.ByteString.Arbitrary  ( ArbByteString(..) )
import Data.Map                   ( Map )
import Data.ObjectID              ( ObjectID )
import Data.Serialize             ( Serialize, encode, decode )
import Data.Text                  ( Text )
import Test.Tasty
import Test.Tasty.QuickCheck      ( Arbitrary, testProperty, oneof, arbitrary )
import Test.QuickCheck            ( Gen, elements )

-- import Debug.Trace ( trace )
-- trace :: a -> b -> b
-- trace _ b = b

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

  int_int :: [(Int,Int)] -> Gen Bool
  int_int pairs =
    let m = Map.fromList pairs
        st = ST.fromMap m
    in if m == ST.toMap st && Map.keys m == ST.keys st && Map.elems m == ST.elems st
      then do
        and <$> sequence [ test_delete pairs
                         , return $ test_lookup (Map.toList m) st
                         , test_insert pairs
                         , test_mutate st ]
      else return False

  float_int :: [(Float,Int)] -> Gen Bool
  float_int pairs =
    let m = Map.fromList pairs
        st = ST.fromMap m
    in if m == ST.toMap st && Map.keys m == ST.keys st && Map.elems m == ST.elems st
      then do
        and <$> sequence [ test_delete pairs
                         , return $ test_lookup (Map.toList m) st
                         , test_insert pairs
                         , test_mutate st ]
      else return False

  bytestring_int :: [(ArbByteString,Int)] -> Gen Bool
  bytestring_int pairs =
    let p' = map (first fromABS) pairs
        m = Map.fromList p'
        st = ST.fromMap m
    in if m == ST.toMap st && Map.keys m == ST.keys st && Map.elems m == ST.elems st
      then do
        and <$> sequence [ test_delete p'
                         , return $ test_lookup (Map.toList m) st
                         , test_insert p' ]
      else return False

  test_lookup :: (Ord k, Show k, Eq v, Show v) => [(k,v)] -> StableTree k v -> Bool
  test_lookup [] _ = True
  test_lookup ((k,v):rest) t =
    case ST.lookup k t of
      Just v' | v' == v -> test_lookup rest t
      _ -> False

  test_delete :: ( Eq k, Show k, Ord k, Serialize k, StableKey k
                 , Eq v, Show v, Serialize v)
              => [(k,v)]
              -> Gen Bool
  test_delete [] = return True
  test_delete kvs = do
    delkey <- elements (map fst kvs)
    let m  = Map.fromList kvs
        m' = Map.delete delkey m
        s  = ST.fromMap m
        s' = ST.delete delkey s
    return (s' == ST.fromMap m')

  test_insert :: ( Eq k, Show k, Ord k, Serialize k, StableKey k
                 , Eq v, Show v, Serialize v)
              => [(k,v)]
              -> Gen Bool
  test_insert [] = return True
  test_insert kvs = do
    (inskey, insval) <- elements kvs
    let kvs' = [(k,v) | (k,v) <- kvs, k /= inskey]
        m    = Map.fromList kvs'
        m'   = Map.insert inskey insval m
        s    = ST.fromMap m
        s'   = ST.insert inskey insval s
    return (s' == ST.fromMap m')

  test_mutate :: ( Arbitrary k, Eq k, Show k, Ord k, Serialize k, StableKey k
                 , Arbitrary v, Eq v, Show v, Serialize v)
              => StableTree k v
              -> Gen Bool
  test_mutate tree | ST.size tree == 0 = return True
  test_mutate tree = mutate (10::Int) (Set.fromList $ ST.keys tree) tree tree
    where
      mutate 0 _ reference accum = return $ reference == accum
      mutate n keys ref accum = do
        accum' <- oneof [insert, delete]
        mutate (n-1) keys ref accum'
        where
        insert = do
          key <- arbitrary
          if Set.member key keys
            then insert
            else do
              val <- arbitrary
              return $ ST.delete key $ ST.insert key val accum

        delete = do
          key <- elements $ ST.keys accum
          let Just val = ST.lookup key accum
          return $ ST.insert key val $ ST.delete key accum

  store_int_int :: [(Int,Int)] -> Bool
  store_int_int = action

  store_float_int :: [(Float,Int)] -> Bool
  store_float_int = action

  store_bytestring_int :: [(ArbByteString,Int)] -> Bool
  store_bytestring_int = action . map (first fromABS)

  action :: (Eq k, Ord k, Serialize k, StableKey k, Eq v, Serialize v)
         => [(k,v)] -> Bool
  action pairs = fst $ runState go Map.empty
    where
    go = do
      let m  = Map.fromList pairs
          st = ST.fromMap m
      Right tid <- SP.store' store st
      Right st' <- SP.load' load tid
      return $ m == ST.toMap st'

-- |Error type for RAM storage. Not a lot can go wrong in RAM...
data RamError = NotFound ObjectID
              | SerializationError String
              | ApiError Text
              deriving ( Show )

instance Error RamError where
  stableTreeError = ApiError

type StableTreeState = State (Map ByteString ByteString)

store :: (Ord k, Serialize k, StableKey k, Serialize v)
      => ObjectID -> Fragment k v -> StableTreeState (Maybe RamError)
store oid frag = do
  modify $ Map.insert (encode oid) (encode frag)
  return Nothing

load :: (Ord k, Serialize k, StableKey k, Serialize v)
     => ObjectID -> StableTreeState (Either RamError (Fragment k v))
load oid =
  gets (Map.lookup $ encode oid) >>= \case
    Nothing -> return $ Left $ NotFound oid
    Just fragBS ->
      case decode fragBS of
        Left err -> return $ Left $ SerializationError err
        Right frag -> return $ Right frag


