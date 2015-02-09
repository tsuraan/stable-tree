-- |
-- Module    : Data.StableTree.Key
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- Tools for working with StableTree keys.
module Data.StableTree.Key
( Key(fromKey)
, SomeKey(..)
, StableKey(..)
, Terminal
, Nonterminal
, wrap
, unwrap
, hashBs
) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Bits       ( (.&.), shiftR, xor )
import Data.ByteString ( ByteString )
import Data.Int        ( Int8, Int16, Int32, Int64 )
import Data.IntMap     ( IntMap )
import Data.IntSet     ( IntSet )
import Data.Map        ( Map )
import Data.Ratio      ( Ratio )
import Data.Sequence   ( Seq )
import Data.Serialize  ( Serialize, encode )
import Data.Set        ( Set )
import Data.Tree       ( Tree )
import Data.Word       ( Word, Word8, Word16, Word32, Word64 )

-- |Used to indicate that a 'Key' is terminal
data Terminal

-- |Used to indicate that a 'Key' is not terminal
data Nonterminal

-- |A wrapper for keys; this has an ephemeral 't' that will be either
-- 'Terminal' or 'Nonterminal' depending on the result of @byte k@.
newtype Key t k = Key { fromKey :: k } deriving ( Eq, Ord, Show )

-- |A sum type to contain either a 'Terminal' or a 'Nonterminal' 'Key'
data SomeKey k = SomeKey_T (Key Terminal k)
               | SomeKey_N (Key Nonterminal k)
               deriving ( Eq, Ord, Show )

-- |Type class for 'StableTree' keys
class StableKey k where
  -- |Get the "hash" of a key; this is just a single-byte, of which only four
  -- bits are really used. Just enough to allow one key in 16 to be 'Terminal'
  hash :: k -> Word8

-- |Do the magic of wrapping up a key into a 'SomeKey'
wrap :: StableKey k => k -> SomeKey k
wrap k =
  let w8 = hash k
      x  = w8 `xor` (w8 `shiftR` 4)
      w4 = x .&. 0xf
  in if w4 == 0xf
    then SomeKey_T $ Key k
    else SomeKey_N $ Key k

-- |Extract the original key from a wrapped one
unwrap :: SomeKey k -> k
unwrap (SomeKey_T (Key k)) = k
unwrap (SomeKey_N (Key k)) = k

instance StableKey Bool where
  hash = hashBs . encode

instance StableKey Char where
  hash = hashBs . encode

instance StableKey Double where
  hash = hashBs . encode

instance StableKey Float where
  hash = hashBs . encode

instance StableKey Int where
  hash = hashBs . encode

instance StableKey Int8 where
  hash = hashBs . encode

instance StableKey Int16 where
  hash = hashBs . encode

instance StableKey Int32 where
  hash = hashBs . encode

instance StableKey Int64 where
  hash = hashBs . encode

instance StableKey Integer where
  hash = hashBs . encode

instance StableKey Ordering where
  hash = hashBs . encode

instance StableKey Word where
  hash = hashBs . encode

instance StableKey Word8 where
  hash = hashBs . encode

instance StableKey Word16 where
  hash = hashBs . encode

instance StableKey Word32 where
  hash = hashBs . encode

instance StableKey Word64 where
  hash = hashBs . encode

instance StableKey ByteString where
  hash = hashBs . encode

instance StableKey LBS.ByteString where
  hash = hashBs . encode

instance StableKey IntSet where
  hash = hashBs . encode

instance Serialize a => StableKey [a] where
  hash = hashBs . encode

instance (Serialize a, Integral a) => StableKey (Ratio a) where
  hash = hashBs . encode

instance Serialize a => StableKey (Maybe a) where
  hash = hashBs . encode

instance Serialize e => StableKey (IntMap e) where
  hash = hashBs . encode

instance (Ord a, Serialize a) => StableKey (Set a) where
  hash = hashBs . encode

instance Serialize e => StableKey (Tree e) where
  hash = hashBs . encode

instance Serialize e => StableKey (Seq e) where
  hash = hashBs . encode

instance (Serialize a, Serialize b) => StableKey (Either a b) where
  hash = hashBs . encode

instance (Serialize a, Serialize b) => StableKey (a, b) where
  hash = hashBs . encode

instance (Ord k, Serialize k, Serialize e) => StableKey (Map k e) where
  hash = hashBs . encode

instance (Serialize a, Serialize b, Serialize c) => StableKey (a, b, c) where
  hash = hashBs . encode

instance (Serialize a, Serialize b, Serialize c, Serialize d) => StableKey (a, b, c, d) where
  hash = hashBs . encode

instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e) => StableKey (a, b, c, d, e) where
  hash = hashBs . encode

instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e, Serialize f) => StableKey (a, b, c, d, e, f) where
  hash = hashBs . encode

instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e, Serialize f, Serialize g) => StableKey (a, b, c, d, e, f, g) where
  hash = hashBs . encode

instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e, Serialize f, Serialize g, Serialize h) => StableKey (a, b, c, d, e, f, g, h) where
  hash = hashBs . encode

instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e, Serialize f, Serialize g, Serialize h, Serialize i) => StableKey (a, b, c, d, e, f, g, h, i) where
  hash = hashBs . encode

instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e, Serialize f, Serialize g, Serialize h, Serialize i, Serialize j) => StableKey (a, b, c, d, e, f, g, h, i, j) where
  hash = hashBs . encode


-- |Calculate a single-byte hash for a 'ByteString'
hashBs :: ByteString -> Word8
hashBs bs =
  let fnv = fnv1a bs
      w32 = fnv `xor` (fnv `shiftR` 32)
      w16 = w32 `xor` (w32 `shiftR` 16)
      w8  = w16 `xor` (w16 `shiftR` 8)
  in toEnum $ fromEnum $ 0xff .&. w8

fnv1a :: ByteString -> Word64
fnv1a = BS.foldl upd basis
  where
  upd hsh oct = prime * (hsh `xor` (toEnum $ fromEnum oct))
  prime       = 1099511628211
  basis       = 14695981039346656037

