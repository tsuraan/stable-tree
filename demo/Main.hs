-- |A small demo program to demonstrate how stable trees behave compared with
-- how one might naively implement versioned trees in a relational database.
-- More detailed explanation of the naive storage is in the stupidCount
-- function.
module Main
( main
) where

import Data.StableTree        ( fromMap )
import Data.StableTree.IO     ( store )
import Data.StableTree.IO.Ram ( storage )

import qualified Data.Map as Map
import Data.IORef ( readIORef )

-- |Make a ton of related maps, storing all of them in a RAM store and printing
-- out the total number of unique entries in that store and how many database
-- entries would be required from a naive database implementation every
-- so-often.
main = do
  (s, trees, values) <- storage
  mapM_ (doRun s trees values) [0,100..1000::Int]
  store s (fromMap $ Map.fromList [(a,a+1)|a<-[100..1000]])
  prTrees trees values
  store s (fromMap $ Map.fromList [(a,a+1)|a<-[200..1000]])
  prTrees trees values
  store s (fromMap $ Map.fromList [(a,a+1)|a<-[0..400]++[600..1000]])
  prTrees trees values

  where
  doRun s trees values i = do
    mapM_ (upd s) [i..i+100]
    putStr $ stupidCount (i+100) ++ " "
    prTrees trees values

  upd s i = do
    let m = Map.fromList [(a,a+1) | a <- [0..i]]
        t = fromMap m
    store s t

  prTrees trees values = do
    tnum <- readIORef trees >>= return . Map.size
    tsum <- readIORef trees >>= return . sum . map (Map.size . snd) . Map.elems
    vsum <- readIORef values >>= return . Map.size
    putStrLn $ show (tsum+vsum) ++ " (" ++ show tnum ++ "," ++ show tsum
               ++ "," ++ show vsum ++ ")"

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
