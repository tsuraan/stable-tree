module Main
( main
) where

import qualified Data.StableTree as ST

import qualified Data.Map as Map
import Control.Arrow ( first )
import Data.ByteString.Arbitrary ( ArbByteString(..) )
import Test.Tasty
import Test.Tasty.QuickCheck ( testProperty )

main :: IO ()
main = defaultMain $
  testGroup "StableTree"
  [ testGroup "Pure"
    [ testProperty "Int/Int" int_int
    , testProperty "Float/Int" float_int
    , testProperty "ByteString/Int" bytestring_int
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

