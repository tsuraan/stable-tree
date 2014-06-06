-- |
-- Module    : Data.StableTree.Types.Key
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- Tools for working with StableTree keys. Just about anything can be a key, so
-- long as there's a sane way to implement IsKey and the standard Ord class.
--
-- Typical users don't need to worry about anything here other than perhaps
-- IsKey.
module Data.StableTree.Types.Key
( IsKey(..)
, Key(fromKey)
, SomeKey(..)
, Terminal
, Nonterminal
, wrap
, unwrap
, hashSerialize
, hashBinary
, hashByteString
) where

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as BS
import qualified Data.Serialize  as S
import qualified Data.Binary as B
import Data.Bits       ( (.&.), shiftR, xor )
import Data.ByteString ( ByteString )
import Data.Int        ( Int8, Int16, Int32, Int64 )
import Data.Word       ( Word, Word8, Word16, Word32, Word64 )

-- |Used to indicate that a 'Key' is terminal
data Terminal

-- |Used to indicate that a 'Key' is not terminal
data Nonterminal

-- |A wrapper for keys; this has an ephemeral 't' that will be either
-- 'Terminal' or 'Nonterminal' depending on the result of @hash k@.
newtype Key t k = Key { fromKey :: k } deriving ( Eq, Ord, Show )

-- |A sum type to contain either a 'Terminal' or a 'Nonterminal' 'Key'
data SomeKey k = SomeKey_T (Key Terminal k)
               | SomeKey_N (Key Nonterminal k)
               deriving ( Eq, Ord, Show )

-- |Do the magic of wrapping up a key into a 'SomeKey'
wrap :: IsKey k => k -> SomeKey k
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

-- |Calculate a hash for an instance of 'S.Serialize'
hashSerialize :: S.Serialize t => t -> Word8
hashSerialize = hashByteString . S.encode

-- |Calculate a hash for an instance of 'B.Binary'
hashBinary :: B.Binary t => t -> Word8
hashBinary = hashByteString . Lazy.toStrict . B.encode

-- |Calculate a hash for a 'ByteString'
hashByteString :: ByteString -> Word8
hashByteString bs =
  let fnv = fnv1a bs
      w32 = fnv `xor` (fnv `shiftR` 32)
      w16 = w32 `xor` (w32 `shiftR` 16)
      w8  = w16 `xor` (w16 `shiftR` 8)
  in toEnum $ fromEnum $ 0xff .&. w8

-- | Type class for anything that we can use as a key. The goal here is to wrap
-- up a function that can generate a high-entropy eight-bit "hash". Speed is
-- somewhat important here, but since we only actually look at four bits of the
-- hash, it really shouldn't be a problem to quickly generate sufficiently
-- random data.
--
-- Implementors probably want to use 'hashSerialize', 'hashBinary', or
-- 'hashByteString' when writing their 'hash' functions.
class IsKey k where
  -- |Generate an 8-bit hash
  hash :: k -> Word8

instance IsKey Char where
  hash = hashSerialize

instance IsKey Double where
  hash = hashSerialize

instance IsKey Float where
  hash = hashSerialize

instance IsKey Int where
  hash = hashSerialize

instance IsKey Int8 where
  hash = hashSerialize

instance IsKey Int16 where
  hash = hashSerialize

instance IsKey Int32 where
  hash = hashSerialize

instance IsKey Int64 where
  hash = hashSerialize

instance IsKey Integer where
  hash = hashSerialize

instance IsKey Word where
  hash = hashSerialize

instance IsKey Word8 where
  hash = hashSerialize

instance IsKey Word16 where
  hash = hashSerialize

instance IsKey Word32 where
  hash = hashSerialize

instance IsKey Word64 where
  hash = hashSerialize

instance IsKey ByteString where
  hash = hashByteString

instance IsKey Lazy.ByteString where
  hash = hashByteString . Lazy.toStrict

fnv1a :: ByteString -> Word64
fnv1a = BS.foldl upd basis
  where
  upd hsh oct = prime * (hsh `xor` (toEnum $ fromEnum oct))
  prime       = 1099511628211
  basis       = 14695981039346656037

