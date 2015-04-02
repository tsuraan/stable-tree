{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving  #-}
module Main
( main
) where

import qualified Data.StableTree as ST
import qualified Data.StableTree.Store as SS
import Data.StableTree ( StableTree )
import Data.StableTree.Store ( Fragment(..), StoreError(..) )
import Data.StableTree.Key     ( StableKey )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow              ( first )
import Control.Applicative        ( Applicative, (<$>) )
import Control.Monad.State.Strict ( MonadState, State, runState, modify, gets )
import Control.Monad.Except       ( MonadError, ExceptT(..), runExceptT, throwError )
import Data.ByteString            ( ByteString )
import Data.ByteString.Arbitrary  ( ArbByteString(..) )
import Data.Map                   ( Map )
import Data.ObjectID              ( ObjectID, calculateByteString )
import Data.Serialize             ( Serialize, encode, decode )
import Test.Tasty
import Test.Tasty.QuickCheck      ( Arbitrary, testProperty, oneof, arbitrary )
import Test.QuickCheck            ( Gen, elements )

-- import Debug.Trace ( trace )
trace :: a -> b -> b
trace = flip const

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
  action pairs =
    case runState (runExceptT (fromSTS go)) Map.empty of
      (Right bool, _) -> bool
      (Left err, _) -> trace err False
    where
    go = do
      let m  = Map.fromList pairs
          st = ST.fromMap m
      tid <- SS.store' store st
      st' <- SS.load' load tid
      return $ m == ST.toMap st'

data RamError = NotFound ObjectID
              | SerializationError String
              | ApiError String
              deriving ( Show )

type S = Map ByteString ByteString
newtype StableTreeState a = STS {
  fromSTS :: ExceptT RamError (State S) a
  }
  deriving ( Applicative, Functor, Monad, MonadError RamError, MonadState S )

instance SS.StoreError StableTreeState where
  serializeError e = throwError $ ApiError e
  reconstitutionError e = throwError $ ApiError e

store :: (Ord k, Serialize k, StableKey k, Serialize v)
      => Fragment ObjectID k v -> StableTreeState ObjectID
store frag = do
  let frag' = encode frag
      oid   = calculateByteString frag'
  modify $ Map.insert (encode oid) frag'
  return oid

load :: (Ord k, Serialize k, StableKey k, Serialize v)
     => ObjectID -> StableTreeState (Fragment ObjectID k v)
load oid =
  gets (Map.lookup $ encode oid) >>= \case
    Nothing -> throwError $ NotFound oid
    Just fragBS ->
      case decode fragBS of
        Left err -> throwError $ SerializationError err
        Right frag -> return frag


