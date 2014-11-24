{-# LANGUAGE LambdaCase, OverloadedStrings, GADTs, ExistentialQuantification, StandaloneDeriving #-}
-- |
-- Module    : Data.StableTree.Types
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- Definitions of primitive types used in different modules of stable-tree
module Data.StableTree.Types
( Depth
, ValueCount
, StableTree(..)
, Incomplete
, Complete
, Z
, S
, Tree(..)
, Fragment(..)
, mkBottom
, mkIBottom0
, mkIBottom1
, mkBranch
, mkIBranch0
, mkIBranch1
, mkIBranch2
, getObjectID
, getDepth
, getValueCount
, calcObjectID
, fixObjectID
, makeFragment
) where

import qualified Data.StableTree.Key as Key
import Data.StableTree.Key      ( SomeKey(..), Key(..), Terminal, Nonterminal )

import qualified Data.Map as Map
import Control.Applicative ( (<$>) )
import Control.Arrow      ( second )
import Control.Monad      ( replicateM )
import Data.Serialize     ( Serialize(..) )
import Data.Serialize.Put ( Put, putByteString )
import Data.Serialize.Get ( Get, getByteString )
import Data.ObjectID  ( ObjectID, calculateSerialize )
import Data.Map       ( Map )

-- |Alias to indicate how deep a branch in a tree is. Bottoms have depth 0
type Depth = Int

-- |Alias that indicates the total number of values underneath a tree
type ValueCount = Int

-- | @StableTree@ is the user-visible type that wraps the actual 'Tree'
-- implementation. All the public functions operate on this type.
data StableTree k v = forall d. StableTree_I (Tree d Incomplete k v)
                    | forall d. StableTree_C (Tree d Complete k v)

-- |Used to indicate that a 'Tree' is not complete
data Incomplete 

-- |Used to indicate that a 'Tree' is complete
data Complete   

-- |Empty type to indicate a Tree with Zero depth (a bottom node)
data Z

-- |Empty type to indicate a Tree with some known height (a branch)
data S a

-- |The actual Rose Tree structure. StableTree is built on one main idea: every
-- 'Key' is either 'Terminal' or 'Nonterminal'. A complete 'Tree' is one whose
-- final element's Key is terminal, and the rest of the Keys are not (exept for
-- two freebies at the beginning to guarantee convergence). A complete tree
-- always has complete children.
--
-- If we don't have enough data to generate a complete tree (i.e. we ran out of
-- elements before hitting a terminal key), then an 'Incomplete' tree is
-- generated. Incomplete trees are always contained by other incomplete trees,
-- and a tree built from only the complete children of an incomplete tree would
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
-- about whether the keys are terminal or not), and then a run of nonterminal
-- keys, and a final, terminal key. Under this scheme, inserting a new entry
-- into a branch will probably mean inserting a nonterminal key, and it will
-- probably be inserted into the run of nonterminal children. If that's the
-- case, no neighbors will be affected, and only the parents will have to
-- change to point to the new branch. Stability is achieved!
data Tree d c k v where
  Bottom :: ObjectID
         -> (SomeKey k, v)
         -> (SomeKey k, v)
         -> Map (Key Nonterminal k) v
         -> (Key Terminal k, v)
         -> Tree Z Complete k v

  -- Either an empty or a singleton tree
  IBottom0 :: ObjectID
           -> Maybe (SomeKey k, v)
           -> Tree Z Incomplete k v

  -- Any number of items, but not ending with a terminal key
  IBottom1 :: ObjectID
           -> (SomeKey k, v)
           -> (SomeKey k, v)
           -> Map (Key Nonterminal k) v
           -> Tree Z Incomplete k v

  Branch :: ObjectID
         -> Depth
         -> (SomeKey k, ValueCount, Tree d Complete k v)
         -> (SomeKey k, ValueCount, Tree d Complete k v)
         -> Map (Key Nonterminal k) (ValueCount, Tree d Complete k v)
         -> (Key Terminal k, ValueCount, Tree d Complete k v)
         -> Tree (S d) Complete k v

  -- A strut to lift an incomplete tree to the next level up
  IBranch0 :: ObjectID
           -> Depth
           -> (SomeKey k, ValueCount, Tree d Incomplete k v)
           -> Tree (S d) Incomplete k v

  -- A joining of a single complete and maybe an incomplete
  IBranch1 :: ObjectID
           -> Depth
           -> (SomeKey k, ValueCount, Tree d Complete k v)
           -> Maybe (SomeKey k, ValueCount, Tree d Incomplete k v)
           -> Tree (S d) Incomplete k v

  -- A branch that doesn't have a terminal, and that might have an IBranch
  IBranch2 :: ObjectID
           -> Depth
           -> (SomeKey k, ValueCount, Tree d Complete k v)
           -> (SomeKey k, ValueCount, Tree d Complete k v)
           -> Map (Key Nonterminal k) (ValueCount, Tree d Complete k v)
           -> Maybe (SomeKey k, ValueCount, Tree d Incomplete k v)
           -> Tree (S d) Incomplete k v

mkBottom :: (Ord k, Serialize k, Serialize v)
         => (SomeKey k, v) -> (SomeKey k, v) -> Map (Key Nonterminal k) v
         -> (Key Terminal k, v) -> Tree Z Complete k v
mkBottom p1 p2 nts t = fixObjectID $ Bottom undefined p1 p2 nts t

mkIBottom0 :: (Ord k, Serialize k, Serialize v)
           => Maybe (SomeKey k, v) -> Tree Z Incomplete k v
mkIBottom0 mp = fixObjectID $ IBottom0 undefined mp

mkIBottom1 :: (Ord k, Serialize k, Serialize v)
           => (SomeKey k, v) -> (SomeKey k, v) -> Map (Key Nonterminal k) v
           -> Tree Z Incomplete k v
mkIBottom1 p1 p2 nts = fixObjectID $ IBottom1 undefined p1 p2 nts

mkBranch :: (Ord k, Serialize k, Serialize v)
         => Depth
         -> (SomeKey k, ValueCount, Tree d Complete k v)
         -> (SomeKey k, ValueCount, Tree d Complete k v)
         -> Map (Key Nonterminal k) (ValueCount, Tree d Complete k v)
         -> (Key Terminal k, ValueCount, Tree d Complete k v)
         -> Tree (S d) Complete k v
mkBranch d t1 t2 nts t = fixObjectID $ Branch undefined d t1 t2 nts t

mkIBranch0 :: (Ord k, Serialize k, Serialize v)
           => Depth
           -> (SomeKey k, ValueCount, Tree d Incomplete k v)
           -> Tree (S d) Incomplete k v
mkIBranch0 d inc = fixObjectID $ IBranch0 undefined d inc

  -- A joining of a single complete and maybe an incomplete
mkIBranch1 :: (Ord k, Serialize k, Serialize v)
           => Depth
           -> (SomeKey k, ValueCount, Tree d Complete k v)
           -> Maybe (SomeKey k, ValueCount, Tree d Incomplete k v)
           -> Tree (S d) Incomplete k v
mkIBranch1 d tup minc = fixObjectID $ IBranch1 undefined d tup minc

  -- A branch that doesn't have a terminal, and that might have an IBranch
mkIBranch2 :: (Ord k, Serialize k, Serialize v)
           => Depth
           -> (SomeKey k, ValueCount, Tree d Complete k v)
           -> (SomeKey k, ValueCount, Tree d Complete k v)
           -> Map (Key Nonterminal k) (ValueCount, Tree d Complete k v)
           -> Maybe (SomeKey k, ValueCount, Tree d Incomplete k v)
           -> Tree (S d) Incomplete k v
mkIBranch2  d t1 t2 nts minc = fixObjectID $ IBranch2 undefined d t1 t2 nts minc

-- |A 'Fragment' is a user-visible part of a tree, i.e. a single node in the
-- tree that can actually be manipulated by a user. This is useful when doing
-- the work of persisting trees, and its serialize instance is also used to
-- calculate Tree ObjectIDs
data Fragment k v
  = FragmentBranch
    { fragmentDepth    :: Depth
    , fragmentChildren :: Map k (ValueCount, ObjectID)
    }
  | FragmentBottom
    { fragmentMap :: Map k v
    }
  deriving( Eq, Ord, Show )

class TreeNode n where
  getObjectID   :: n k v -> ObjectID
  getDepth      :: n k v -> Depth
  getValueCount :: n k v -> ValueCount
  calcObjectID  :: (Ord k, Serialize k, Serialize v) => n k v -> ObjectID
  fixObjectID   :: (Ord k, Serialize k, Serialize v) => n k v -> n k v
  makeFragment  :: Ord k => n k v -> Fragment k v
  -- getFullContents :: n k v -> Map k v

instance TreeNode (Tree d c) where
  getObjectID (Bottom o _ _ _ _)     = o
  getObjectID (IBottom0 o _)         = o
  getObjectID (IBottom1 o _ _ _)     = o
  getObjectID (Branch o _ _ _ _ _)   = o
  getObjectID (IBranch0 o _ _)       = o
  getObjectID (IBranch1 o _ _ _)     = o
  getObjectID (IBranch2 o _ _ _ _ _) = o

  getDepth (Bottom _ _ _ _ _)     = 0
  getDepth (IBottom0 _ _)         = 0
  getDepth (IBottom1 _ _ _ _)     = 0
  getDepth (Branch _ d _ _ _ _)   = d
  getDepth (IBranch0 _ d _)       = d
  getDepth (IBranch1 _ d _ _)     = d
  getDepth (IBranch2 _ d _ _ _ _) = d

  getValueCount (Bottom _ _ _ m _)   = 3 + Map.size m
  getValueCount (IBottom0 _ Nothing) = 0
  getValueCount (IBottom0 _ _)       = 1
  getValueCount (IBottom1 _ _ _ m)   = 2 + Map.size m
  
  getValueCount (Branch _ _ (_,c1,_) (_,c2,_) nterm (_,c3,_)) =
    c1 + c2 + c3 + sum (map fst $ Map.elems nterm)
  getValueCount (IBranch0 _ _ (_,c,_)) =
    c
  getValueCount (IBranch1 _ _ (_,c,_) Nothing) =
    c
  getValueCount (IBranch1 _ _ (_,c1,_) (Just (_,c2,_))) =
    c1+c2
  getValueCount (IBranch2 _ _ (_,c1,_) (_,c2,_) m i) =
    c1 + c2 + sum (map fst $ Map.elems m) + maybe 0 (\(_,c3,_)->c3) i

  calcObjectID tree = calculateSerialize $ makeFragment tree

  fixObjectID t@(Bottom _ a b c d)     = Bottom (calcObjectID t) a b c d
  fixObjectID t@(IBottom0 _ a)         = IBottom0 (calcObjectID t) a
  fixObjectID t@(IBottom1 _ a b c)     = IBottom1 (calcObjectID t) a b c
  fixObjectID t@(Branch _ a b c d e)   = Branch (calcObjectID t) a b c d e
  fixObjectID t@(IBranch0 _ a b)       = IBranch0 (calcObjectID t) a b
  fixObjectID t@(IBranch1 _ a b c)     = IBranch1 (calcObjectID t) a b c
  fixObjectID t@(IBranch2 _ a b c d e) = IBranch2 (calcObjectID t) a b c d e

  makeFragment tree =
    case tree of
      (Bottom _ p1 p2 m pt) ->
        fragBottom p1 p2 m (Just pt)
      (IBottom0 _ Nothing) ->
        FragmentBottom Map.empty
      (IBottom0 _ (Just (k1,v1))) ->
        FragmentBottom $ Map.singleton (Key.unwrap k1) v1
      (IBottom1 _ p1 p2 m) ->
        fragBottom p1 p2 m Nothing
      (Branch _ d (k1,c1,t1) (k2,c2,t2) m (kt,ct,tt)) ->
        let cont = Map.insert (Key.unwrap k1) (c1,getObjectID t1)
                 $ Map.insert (Key.unwrap k2) (c2,getObjectID t2)
                 $ Map.insert (fromKey kt) (ct,getObjectID tt)
                 $ Map.mapKeys fromKey
                 $ Map.map (second getObjectID) m
        in FragmentBranch d cont
      (IBranch0 _ d (k,c,t)) ->
        FragmentBranch d $ Map.singleton (Key.unwrap k) (c,getObjectID t)
      (IBranch1 _ d (k,c,t) Nothing) ->
        FragmentBranch d $ Map.singleton (Key.unwrap k) (c,getObjectID t)
      (IBranch1 _ d (k,c,t) (Just (ki,ci,ti))) ->
        let cont = Map.fromList [ (Key.unwrap k, (c, getObjectID t))
                                , (Key.unwrap ki, (ci, getObjectID ti)) ]
        in FragmentBranch d cont
      (IBranch2 _ d (k1,c1,t1) (k2,c2,t2) m minc) ->
        let cont = Map.insert (Key.unwrap k1) (c1,getObjectID t1)
                 $ Map.insert (Key.unwrap k2) (c2,getObjectID t2)
                 $ Map.mapKeys fromKey
                 $ Map.map (second getObjectID) m
            cont' = case minc of
              Nothing -> cont
              (Just (ki,ci,ti)) ->
                Map.insert (Key.unwrap ki) (ci, getObjectID ti) cont
        in FragmentBranch d cont'
    where
    fragBottom (k1,v1) (k2,v2) mapping mterm =
      let cont = Map.insert (Key.unwrap k1) v1
               $ Map.insert (Key.unwrap k2) v2
               $ Map.mapKeys fromKey mapping
          cont' = case mterm of
            Nothing -> cont
            (Just (tk, tv)) -> Map.insert (fromKey tk) tv cont
      in FragmentBottom cont'

instance TreeNode StableTree where
  getObjectID (StableTree_I t) = getObjectID t
  getObjectID (StableTree_C t) = getObjectID t

  getDepth (StableTree_I t) = getDepth t
  getDepth (StableTree_C t) = getDepth t

  getValueCount (StableTree_I t) = getValueCount t
  getValueCount (StableTree_C t) = getValueCount t

  calcObjectID (StableTree_I t) = calcObjectID t
  calcObjectID (StableTree_C t) = calcObjectID t

  fixObjectID (StableTree_I t) = StableTree_I $ fixObjectID t
  fixObjectID (StableTree_C t) = StableTree_C $ fixObjectID t

  makeFragment (StableTree_I t) = makeFragment t
  makeFragment (StableTree_C t) = makeFragment t

instance Eq (Tree d c k v) where
  t1 == t2 = getObjectID t1 == getObjectID t2

instance Eq (StableTree k v) where
  (StableTree_I t1) == (StableTree_I t2) = getObjectID t1 == getObjectID t2
  (StableTree_C t1) == (StableTree_C t2) = getObjectID t1 == getObjectID t2
  (StableTree_I _) == (StableTree_C _) = False
  (StableTree_C _) == (StableTree_I _) = False

instance Ord (StableTree k v) where
  compare l r = compare (getObjectID l) (getObjectID r)

deriving instance (Ord k, Show k, Show v) => Show (StableTree k v)
deriving instance (Ord k, Show k, Show v) => Show (Tree d c k v)

instance (Ord k, Serialize k, Serialize v) => Serialize (Fragment k v) where
  put frag =
    case frag of
      (FragmentBranch depth children) -> fragPut depth children
      (FragmentBottom values)         -> fragPut 0 values
    where
    fragPut :: (Serialize k, Serialize v) => Depth -> Map k v -> Put
    fragPut depth items = do
      putByteString "stable-tree\0"
      put depth
      put $ Map.size items
      mapM_ (\(k,v) -> put k >> put v) (Map.toAscList items)

  get =
    getByteString 12 >>= \case
      "stable-tree\0" -> do
        get >>= \case
          0 -> do
            count <- get
            children <- Map.fromList <$> replicateM count getPair
            return $ FragmentBottom children
          depth -> do
            count <- get
            children <- Map.fromList <$> replicateM count getPair
            return $ FragmentBranch depth children
      _ -> fail "Not a serialized Fragment"
    where
    getPair :: (Serialize k, Serialize v) => Get (k,v)
    getPair = do
      k <- get
      v <- get
      return (k,v)

