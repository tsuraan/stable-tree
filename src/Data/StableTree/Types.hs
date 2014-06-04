{-# LANGUAGE GADTs #-}
-- |
-- Module    : Data.StableTree.Types
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- This is the core implementation of the stable tree. The primary functions
-- exported by this module are 'nextBottom' and 'nextBranch', which gather
-- values or lower-level 'Tree's into 'Tree's of the next level.
--
-- This module is fairly esoteric. "Data.StableTree" or "Data.StableTree.IO"
-- are probably what you actually want to be using.
module Data.StableTree.Types
( IsKey(..)
, Tree(..)
, Complete
, Incomplete
, nextBottom
, nextBranch
, getKey
, completeKey
, treeContents
) where

import Data.StableTree.Types.Key

import qualified Data.Map as Map
import Control.Arrow ( first, second )
import Data.Map ( Map )
import Data.List ( intercalate )

-- |Used to indicate that a 'Tree' is not complete
data Incomplete 

-- |Used to indicate that a 'Tree' is complete
data Complete   

-- |The actual Rose Tree structure. StableTree is built on one main idea: every
-- 'Key' is either 'Terminal' or 'Nonterminal'. A complete 'Tree' is one whose
-- final element's Key is terminal, and the rest of the Keys are not (exept for
-- two freebies at the beginning to guarantee convergence). A complete tree
-- always has complete children.
--
-- If we don't have enough data to generate a complete tree (i.e. we ran out of
-- elements before hitting a terminal key), then an 'Incomplete' tree is
-- generated. Incomplete trees are always contained by other incomplete trees,
-- and a tree built from only the complete chlidren of an incomplete tree would
-- never itself be complete.
--
-- It is easiest to understand how this structure promotes stability by looking
-- at how trees typically work. The easiest tree to understand is a simple,
-- well balanced, binary tree. In that case, we would have a structure like this:
--
-- @
--       |D|
--   |B|     |F|
-- |A| |C| |E| |G|
-- @
--
-- Now, suppose that we want to delete the data stored in @|A|@. Then, we'll
-- get a new structure that shares nothing in common with the original one:
--
-- @
--       |E|
--   |C|     |G|
-- |B| |D| |F|
-- @
--
-- The entire tree had to be re-written. This structure is clearly unstable
-- under mutation. Making the tree wider doesn't help much if the tree's size
-- is changing. Simple updates to existing keys are handled well by branches
-- with many children, but deleting from or adding to the beginning of the tree
-- will always cause every single branch to change, which is what this
-- structure is trying to avoid.
--
-- Instead, the stable tree branches have variable child counts. A branch is
-- considered full when its highest key is "terminal", which is determined by
-- hashing the key and looking at some bits of the hash. I've found that a
-- target branch size of 16 children works fairly well, so we check to see if
-- the hash has its least-significant four bits set; if that's the case, the
-- key is terminal. A branch gets two free children (meaning it doesn't care
-- about whether the keys are temrinal or not), and then a run of nonterminal
-- keys, and a final, terminal key. Under this scheme, inserting a new entry
-- into a branch will probably mean inserting a nonterminal key, and it will
-- probably be inserted into the run of nonterminal children. If that's the
-- case, no neighbors will be affected, and only the parents will have to
-- change to point to the new branch. Stability is acheived!
data Tree c k v where
  Bottom :: (SomeKey k, v)
         -> (SomeKey k, v)
         -> Map (Key Nonterminal k) v
         -> (Key Terminal k, v)
         -> Tree Complete k v

  Branch :: Int
         -> (SomeKey k, Tree Complete k v)
         -> (SomeKey k, Tree Complete k v)
         -> Map (Key Nonterminal k) (Tree Complete k v)
         -> (Key Terminal k, Tree Complete k v)
         -> Tree Complete k v

  -- Either an empty or a singleton tree
  IBottom0 :: Maybe (SomeKey k, v)
           -> Tree Incomplete k v

  -- Any number of items, but not ending with a terminal key
  IBottom1 :: (SomeKey k, v)
           -> (SomeKey k, v)
           -> Map (Key Nonterminal k) v
           -> Tree Incomplete k v

  -- A strut to lift an incomplete tree to the next level up
  IBranch0 :: Int
           -> (SomeKey k, Tree Incomplete k v)
           -> Tree Incomplete k v

  -- A joining of a single complete and maybe an incomplete
  IBranch1 :: Int
           -> (SomeKey k, Tree Complete k v)
           -> Maybe (SomeKey k, Tree Incomplete k v)
           -> Tree Incomplete k v

  -- A branch that doesn't have a terminal, and that might have an IBranch
  IBranch2 :: Int
           -> (SomeKey k, Tree Complete k v)
           -> (SomeKey k, Tree Complete k v)
           -> Map (Key Nonterminal k) (Tree Complete k v)
           -> Maybe (SomeKey k, Tree Incomplete k v)
           -> Tree Incomplete k v

-- |Wrap up some of a k/v map into a 'Tree'. A 'Right' result gives a complete
-- tree and the map updated to not have the key/values that went into that
-- tree. A 'Left' result gives an incomplete tree that contains everything that
-- the given map contained.
nextBottom :: (Ord k, IsKey k)
           => Map k v
           -> Either (Tree Incomplete k v)
                     (Tree Complete k v, Map k v)
nextBottom values =
  case Map.minViewWithKey values >>= return . second Map.minViewWithKey of
    Nothing -> Left $ IBottom0 Nothing
    Just ((k,v), Nothing) -> Left $ IBottom0 $ Just (wrap k, v)
    Just (f1, Just (f2, remain)) ->
      go (first wrap f1) (first wrap f2) Map.empty remain

  where
  go f1 f2 accum remain =
    case Map.minViewWithKey remain of
      Nothing ->
        Left $ IBottom1 f1 f2 accum
      Just ((k, v), remain') ->
        case wrap k of
          SomeKey_N nonterm ->
            go f1 f2 (Map.insert nonterm v accum) remain'
          SomeKey_T term ->
            Right (Bottom f1 f2 accum (term, v), remain')

-- |Generate a parent for a k/Tree map. A 'Right' result gives a complete tree
-- and the map updated to not have the key/trees that went into that tree. A
-- 'Left' result gives an incomplete tree that contains everything that the
-- given map contained.
nextBranch :: (Ord k, IsKey k)
           => Map k (Tree Complete k v)
           -> Maybe (k, Tree Incomplete k v)
           -> Either (Tree Incomplete k v)
                     (Tree Complete k v, Map k (Tree Complete k v))
nextBranch branches mIncomplete =
  let freebies = Map.minViewWithKey branches
                 >>= return . second Map.minViewWithKey
  in case freebies of
    Nothing -> 
      case mIncomplete of
        Nothing       -> Left $ IBottom0 Nothing
        Just (ik, iv) -> Left $ IBranch0 depth (wrap ik, iv)
    Just ((k,v), Nothing) ->
      Left $ IBranch1 depth (wrap k,v) $ wrapMKey mIncomplete
    Just (f1, Just (f2, remain)) ->
      go (wrapKey f1) (wrapKey f2) Map.empty remain

  where
  go f1 f2 accum remain =
    let popd = Map.minViewWithKey remain >>= return . first wrapKey
    in case popd of
      Nothing ->
        Left $ IBranch2 depth f1 f2 accum $ wrapMKey mIncomplete
      Just ((SomeKey_T term,v), remain') ->
        Right ( Branch depth f1 f2 accum (term, v), remain' )
      Just ((SomeKey_N nonterm,v), remain') ->
        go f1 f2 (Map.insert nonterm v accum) remain'

  wrapKey :: IsKey k => (k,v) -> (SomeKey k, v)
  wrapKey = first wrap

  wrapMKey :: IsKey k => Maybe (k,v) -> Maybe (SomeKey k, v)
  wrapMKey = (>>=return . wrapKey)

  depth = case Map.elems branches of
    [] ->
      case mIncomplete of
        Nothing -> 1
        Just (_, v) -> 1 + getDepth v
    elems ->
      let depths@(f:r) = map getDepth elems
          (best, rest) = case mIncomplete of
                          Nothing -> (f, r)
                          Just (_, v) -> (getDepth v, depths)
      in if all (==best) rest
        then 1 + best
        else error "Depth mismatch in nextBranch"

-- |Get the key of the first entry in this branch. If the branch is empty,
-- returns Nothing.
getKey :: Tree c k v -> Maybe k
getKey (Bottom (k,_) _ _ _)     = Just $ unwrap k
getKey (Branch _ (k,_) _ _ _)   = Just $ unwrap k
getKey (IBottom0 Nothing)       = Nothing
getKey (IBottom0 (Just (k,_)))  = Just $ unwrap k
getKey (IBottom1 (k,_) _ _)     = Just $ unwrap k
getKey (IBranch0 _ (k,_))       = Just $ unwrap k
getKey (IBranch1 _ (k,_) _)     = Just $ unwrap k
getKey (IBranch2 _ (k,_) _ _ _) = Just $ unwrap k

-- |Get the key of the fist entry in this complete branch. This function is
-- total.
completeKey :: Tree Complete k v -> k
completeKey (Bottom (k,_) _ _ _)     = unwrap k
completeKey (Branch _ (k,_) _ _ _)   = unwrap k

-- |Convert an entire Tree into a k/v map.
treeContents :: Ord k => Tree c k v -> Map k v
treeContents t =
  case branchContents t of
    Left ( completes, Nothing) ->
      Map.unions $ map treeContents $ Map.elems completes
    Left ( completes, Just (_k, iv)) ->
      Map.unions $ treeContents iv:map treeContents (Map.elems completes)
    Right x -> x

getDepth :: Tree c k v -> Int
getDepth (Bottom _ _ _ _)     = 0
getDepth (Branch d _ _ _ _)   = d
getDepth (IBottom0 _)         = 0
getDepth (IBottom1 _ _ _)     = 0
getDepth (IBranch0 d _)       = d
getDepth (IBranch1 d _ _)     = d
getDepth (IBranch2 d _ _ _ _) = d

branchContents :: Ord k
               => Tree c k v
               -> Either ( Map k (Tree Complete k v)
                         , Maybe (k, Tree Incomplete k v))
                         ( Map k v )
branchContents (Bottom (k1,v1) (k2,v2) terms (kt,vt)) =
  let terms' = Map.mapKeys fromKey terms
      conts  = Map.insert (unwrap k1) v1
             $ Map.insert (unwrap k2) v2
             $ Map.insert (fromKey kt) vt
             terms'
  in Right conts
branchContents (Branch _d (k1,v1) (k2,v2) terms (kt,vt)) =
  let terms' = Map.mapKeys fromKey terms
      conts  = Map.insert (unwrap k1) v1
             $ Map.insert (unwrap k2) v2
             $ Map.insert (fromKey kt) vt
             terms'
  in Left (conts, Nothing)
branchContents (IBottom0 Nothing) =
  Right Map.empty
branchContents (IBottom0 (Just (k,v))) =
  Right $ Map.singleton (unwrap k) v
branchContents (IBottom1 (k1,v1) (k2,v2) terms) =
  let terms' = Map.mapKeys fromKey terms
      conts  = Map.insert (unwrap k1) v1
             $ Map.insert (unwrap k2) v2
             terms'
  in Right conts
branchContents (IBranch0 _d incomplete) =
  Left (Map.empty, Just $ first unwrap incomplete)
branchContents (IBranch1 _d (k1,v1) mIncomplete) =
  Left (Map.singleton (unwrap k1) v1, mIncomplete >>= return . first unwrap)
branchContents (IBranch2 _d (k1,v1) (k2,v2) terms mIncomplete) =
  let terms' = Map.mapKeys fromKey terms
      conts  = Map.insert (unwrap k1) v1
             $ Map.insert (unwrap k2) v2
             terms'
  in Left (conts, mIncomplete >>= return . first unwrap)

instance (Ord k, Show k, Show v) => Show (Tree c k v) where
  show t@(Bottom _ _ _ _)     = branchShow "Bottom" t
  show t@(Branch _ _ _ _ _)   = branchShow "Branch" t
  show t@(IBottom0 _)         = branchShow "IBottom" t
  show t@(IBottom1 _ _ _)     = branchShow "IBottom" t
  show t@(IBranch0 _ _)       = branchShow "IBranch" t
  show t@(IBranch1 _ _ _)     = branchShow "IBranch" t
  show t@(IBranch2 _ _ _ _ _) = branchShow "IBranch" t

branchShow :: (Ord k, Show k, Show v) => String -> Tree c k v -> String
branchShow header t =
  case branchContents t of
    Left (ts, Nothing) ->
      let strs = [show k ++ " => " ++ show v | (k, v) <- Map.toAscList ts]
          str  = intercalate ", " strs
      in header ++ "(" ++ show (getDepth t) ++ ")<" ++ str ++ ">"
    Left (ts, Just (ik, iv)) ->
      let strs = [ show k ++ " => " ++ show v | (k, v) <- Map.toAscList ts
                 ] ++ [show ik ++ " => " ++ show iv]
          str  = intercalate ", " strs
      in header ++ "(" ++ show (getDepth t) ++ ")<" ++ str ++ ">"
    Right vals ->
      let strs = [ show k ++ " => " ++ show v | (k, v) <- Map.toAscList vals ]
          str  = intercalate ", " strs
      in header ++ "(" ++ show (getDepth t) ++ ")<" ++ str ++ ">"

