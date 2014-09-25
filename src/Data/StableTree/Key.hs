-- |
-- Module    : Data.StableTree.Types.Key
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- Tools for working with StableTree keys.
module Data.StableTree.Key
( Key(fromKey)
, SomeKey(..)
, Terminal
, Nonterminal
, wrap
, unwrap
) where

import qualified Data.ByteString as BS
import Data.Serialize  ( Serialize, encode )
import Data.Bits       ( (.&.), shiftR, xor )
import Data.ByteString ( ByteString )
import Data.Word       ( Word8, Word64 )

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

-- |Do the magic of wrapping up a key into a 'SomeKey'
wrap :: Serialize k => k -> SomeKey k
wrap k =
  let w8 = byte k
      x  = w8 `xor` (w8 `shiftR` 4)
      w4 = x .&. 0xf
  in if w4 == 0xf
    then SomeKey_T $ Key k
    else SomeKey_N $ Key k

-- |Extract the original key from a wrapped one
unwrap :: SomeKey k -> k
unwrap (SomeKey_T (Key k)) = k
unwrap (SomeKey_N (Key k)) = k

-- |Calculate a single-byte hash for a 'Serialize'
byte :: Serialize t => t -> Word8
byte val =
  let bs  = encode val
      fnv = fnv1a bs
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

