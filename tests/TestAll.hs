module Main
( main
) where

import qualified Data.StableTree as ST
import Data.StableTree.IO ( store, load )
import Data.StableTree.IO.Ram ( storage )

import qualified Data.Map as Map
import Control.Arrow ( first )
import Data.ByteString.Arbitrary ( ArbByteString(..) )
import Test.Tasty
import Test.Tasty.QuickCheck ( testProperty )
import Test.QuickCheck
import Test.QuickCheck.Monadic

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

  store_int_int :: [(Int,Int)] -> Property
  store_int_int pairs = monadicIO $ do
    (s,_,_) <- run storage
    let m = Map.fromList pairs
        st = ST.fromMap m
    Right tid <- run $ store s st
    Right st' <- run $ load s tid
    assert $ m == ST.toMap st'

  store_float_int :: [(Float,Int)] -> Property
  store_float_int pairs = monadicIO $ do
    (s,_,_) <- run storage
    let m  = Map.fromList pairs
        st = ST.fromMap m
    Right tid <- run $ store s st
    Right st' <- run $ load s tid
    assert $ m == ST.toMap st'

  store_bytestring_int :: [(ArbByteString,Int)] -> Property
  store_bytestring_int pairs = monadicIO $ do
    (s,_,_) <- run storage
    let m  = Map.fromList $ map (first fromABS) pairs
        st = ST.fromMap m
    Right tid <- run $ store s st
    Right st' <- run $ load s tid
    assert $ m == ST.toMap st'

