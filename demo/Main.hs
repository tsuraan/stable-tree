{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
-- |A small demo program to demonstrate how stable trees behave compared with
-- how one might naively implement versioned trees in a relational database.
-- More detailed explanation of the naive storage is in the stupidCount
-- function.
module Main
( main
) where

import Data.StableTree
import Data.StableTree.Store

import qualified Data.Map as Map
import Control.Applicative        ( Applicative )
import Control.Monad              ( foldM )
import Control.Monad.State.Strict ( MonadState, State, runState, modify )
import Control.Monad.Except       ( MonadError, ExceptT(..), runExceptT, throwError )
import Data.Map                   ( Map )
import Data.ObjectID              ( ObjectID, calculateSerialize )

type S = Map ObjectID (Fragment ObjectID Int Int)

data DemoError = ApiError String
newtype StableTreeState a = STS {
  fromSTS :: ExceptT DemoError (State S) a
  }
  deriving ( Applicative, Functor, Monad, MonadError DemoError, MonadState S )

instance StoreError StableTreeState where
  serializeError e = throwError $ ApiError e
  reconstitutionError e = throwError $ ApiError e


-- |Make a ton of related maps, storing all of them in a RAM store and printing
-- out the total number of unique entries in that store and how many database
-- entries would be required from a naive database implementation every
-- so-often.
main :: IO ()
main = do
  m0 <- foldM doRun Map.empty [0,100..1000::Int]
  let m1 = upd m0 [100..1000]
  prTrees m1
  let m2 = upd m1 [200..1000]
  prTrees m2
  let m3 = upd m2 $ [0..400] ++ [600..1000]
  prTrees m3

  where
  doRun :: S -> Int -> IO S
  doRun m0 i0 = do
    let m' = foldl (\m i -> upd m [0..i]) m0 [i0..i0+100]
    putStr $ "naive gives: " ++ stupidCount (i0+100) ++ " / stable gives: "
    prTrees m'
    return m'

  upd :: S -> [Int] -> S
  upd m is =
    let t = fromMap $ Map.fromList [(a,a+1) | a <- is]
        (_,m') = runState (runExceptT (fromSTS (save t))) m
    in m'

  save :: StableTree Int Int -> StableTreeState ObjectID
  save = store' writer

  writer frag = do
    let oid = calculateSerialize frag
    modify (Map.insert oid frag)
    return oid

  prTrees m = do
    putStrLn $ (show $ Map.size m) ++ " unique entries"

-- |The typical way of storing key/value maps in SQL is to use a relational
-- table, like this:
--
-- @
-- CREATE TABLE Trees ( id serial primary key, name text );
-- CREATE TABLE Values ( id serial primary key, value bytea );
-- CREATE TABLE tree_entries ( tree_id integer references trees
--                           , value_id integer references values
--                           , name text
--                           , unique(tree_id, name)
--                           );
-- @
--
-- Using this strategy works poorly when trees are related, such as when doing
-- version control on a set of directories. In that case, supposing one were to
-- make a new version every time a file were added to a directory, the size of
-- tree_entries grows as the square of the size of trees. This function does
-- that calculation.
stupidCount :: Int -> String
stupidCount i = show $ i*(i-1) `div` 2
