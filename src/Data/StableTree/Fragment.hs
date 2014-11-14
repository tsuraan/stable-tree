{-# LANGUAGE LambdaCase, OverloadedStrings, GADTs #-}
-- |
-- Module    : Data.StableTree.Types
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- This is the implementation of tree fragments, which are stand-alone chunks
-- of data representing each branch within a 'Data.StableTree.Types.Tree'. This
-- module is also used by the Tree code for generating each branch's
-- 'ObjectID'.
module Data.StableTree.Fragment
( Fragment(..)
) where

import Data.StableTree.Types ( Depth, ValueCount )

import qualified Data.Map as Map
import Control.Monad      ( replicateM )
import Data.ObjectID      ( ObjectID )
import Data.Serialize     ( Serialize(..) )
import Control.Applicative ( (<$>) )
import Data.Serialize.Put ( Put, putByteString )
import Data.Serialize.Get ( Get, getByteString )
import Data.Map           ( Map )

