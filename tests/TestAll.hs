{-# LANGUAGE LambdaCase #-}
module Main
( main
) where

import Data.StableTree ( StableTree )
import qualified Data.StableTree as ST
-- import Data.StableTree ( Fragment, Error(..) )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow              ( first )
import Control.Applicative        ( (<$>) )
-- import Control.Monad.State.Strict ( State, runState, modify, gets )
import Data.ByteString.Arbitrary  ( ArbByteString(..) )
import Data.Map                   ( (!) )
-- import Data.ObjectID              ( ObjectID )
import Data.Serialize             ( Serialize ) -- , encode, decode )
-- import Data.Text                  ( Text )
import Test.Tasty
import Test.Tasty.QuickCheck      ( Arbitrary, testProperty, oneof, arbitrary )
import Test.QuickCheck            ( Gen, elements )

-- import Debug.Trace ( trace )
trace :: a -> b -> b
trace _ b = b

main :: IO ()
main = defaultMain $
  testGroup "StableTree"
  [ testGroup "Pure"
    [ testProperty "Int/Int" int_int
    , testProperty "Float/Int" float_int
    , testProperty "ByteString/Int" bytestring_int
    ]
  {-
  , testGroup "Stored"
    [ testProperty "Int/Int" store_int_int
    , testProperty "Float/Int" store_float_int
    , testProperty "ByteString/Int" store_bytestring_int
    ] -}
  ]
  where

  int_int :: [(Int,Int)] -> Gen Bool
  int_int pairs = trace (show pairs) $
    let m = Map.fromList pairs
        st = ST.fromMap m
    in if m == ST.toMap st
      then do
        and <$> sequence [ test_delete pairs
                         , test_insert pairs
                         , test_mutate st ]
      else trace "early fail" $ return False

  float_int :: [(Float,Int)] -> Gen Bool
  float_int pairs =
    let m = Map.fromList pairs
        st = ST.fromMap m
    in if m == ST.toMap st
      then do
        and <$> sequence [ test_delete pairs
                         , test_insert pairs
                         , test_mutate st ]
      else return False

  bytestring_int :: [(ArbByteString,Int)] -> Gen Bool
  bytestring_int pairs =
    let p' = map (first fromABS) pairs
        m = Map.fromList p'
        st = ST.fromMap m
    in if m == ST.toMap st
      then do
        and <$> sequence [ test_delete p'
                         , test_insert p' ]
      else return False

  test_delete :: (Show k, Ord k, Serialize k, Show v, Serialize v)
              => [(k,v)]
              -> Gen Bool
  test_delete [] = return True
  test_delete kvs = do
    delkey <- trace "hi" $ elements (map fst kvs)
    let m  = trace (show kvs) $ trace (show delkey) $ Map.fromList kvs
        m' = Map.delete delkey m
        s  = ST.fromMap m
        s' = ST.delete delkey s
    trace ("a " ++ show s') $ trace ("b " ++ show (ST.fromMap m')) $ trace ("c " ++ show (s' == ST.fromMap m')) $ return (s' == ST.fromMap m')

  test_insert :: (Show k, Ord k, Serialize k, Show v, Serialize v)
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

  test_mutate :: (Arbitrary k, Show k, Ord k, Serialize k, Arbitrary v, Show v, Serialize v)
              => StableTree k v
              -> Gen Bool
  test_mutate tree | ST.size tree == 0 = return True
  test_mutate tree = mutate (10::Int) (Set.fromList $ Map.keys $ ST.toMap tree) tree tree
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
          let m = ST.toMap accum
          key <- trace (show m) (elements $ Map.keys m)
          let val = m ! key
          return $ ST.insert key val $ ST.delete key accum

  {-
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
      Right tid <- ST.store' store st
      Right st' <- ST.load' load tid
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

-}

