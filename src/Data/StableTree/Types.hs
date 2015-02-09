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
, getDepth
, getValueCount
) where

-- import qualified Data.StableTree.Key as Key
import Data.StableTree.Key      ( SomeKey(..), Key(..), Terminal, Nonterminal )

import qualified Data.Map as Map
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

-- |The actual B-Tree variant. StableTree is built on one main idea: every
-- 'Key' is either 'Terminal' or 'Nonterminal', and every 'Tree' is 'Complete'
-- or 'Incomplete'. A complete 'Tree' is one whose final element's Key is
-- terminal, and the rest of the Keys are not (exept for two freebies at the
-- beginning to guarantee convergence). A complete tree always has complete
-- children.
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
  Bottom :: (SomeKey k, v)
         -> (SomeKey k, v)
         -> Map (Key Nonterminal k) v
         -> (Key Terminal k, v)
         -> Tree Z Complete k v

  -- Either an empty or a singleton tree
  IBottom0 :: Maybe (SomeKey k, v)
           -> Tree Z Incomplete k v

  -- Any number of items, but not ending with a terminal key
  IBottom1 :: (SomeKey k, v)
           -> (SomeKey k, v)
           -> Map (Key Nonterminal k) v
           -> Tree Z Incomplete k v

  Branch :: Depth
         -> (SomeKey k, ValueCount, Tree d Complete k v)
         -> (SomeKey k, ValueCount, Tree d Complete k v)
         -> Map (Key Nonterminal k) (ValueCount, Tree d Complete k v)
         -> (Key Terminal k, ValueCount, Tree d Complete k v)
         -> Tree (S d) Complete k v

  -- A strut to lift an incomplete tree to the next level up
  IBranch0 :: Depth
           -> (SomeKey k, ValueCount, Tree d Incomplete k v)
           -> Tree (S d) Incomplete k v

  -- A joining of a single complete and maybe an incomplete
  IBranch1 :: Depth
           -> (SomeKey k, ValueCount, Tree d Complete k v)
           -> Maybe (SomeKey k, ValueCount, Tree d Incomplete k v)
           -> Tree (S d) Incomplete k v

  -- A branch that doesn't have a terminal, and that might have an IBranch
  IBranch2 :: Depth
           -> (SomeKey k, ValueCount, Tree d Complete k v)
           -> (SomeKey k, ValueCount, Tree d Complete k v)
           -> Map (Key Nonterminal k) (ValueCount, Tree d Complete k v)
           -> Maybe (SomeKey k, ValueCount, Tree d Incomplete k v)
           -> Tree (S d) Incomplete k v

class TreeNode n where
  -- |Get the depth (height?) of a 'Tree' or 'StableTree'
  getDepth      :: n k v -> Depth
  -- |Get the total number of key/value pairs stored under this 'Tree' or
  -- 'StableTree'
  getValueCount :: n k v -> ValueCount

instance TreeNode (Tree d c) where
  getDepth (Bottom _ _ _ _)     = 0
  getDepth (IBottom0 _)         = 0
  getDepth (IBottom1 _ _ _)     = 0
  getDepth (Branch d _ _ _ _)   = d
  getDepth (IBranch0 d _)       = d
  getDepth (IBranch1 d _ _)     = d
  getDepth (IBranch2 d _ _ _ _) = d

  getValueCount (Bottom _ _ m _)   = 3 + Map.size m
  getValueCount (IBottom0 Nothing) = 0
  getValueCount (IBottom0 _)       = 1
  getValueCount (IBottom1 _ _ m)   = 2 + Map.size m
  
  getValueCount (Branch _ (_,c1,_) (_,c2,_) nterm (_,c3,_)) =
    c1 + c2 + c3 + sum (map fst $ Map.elems nterm)
  getValueCount (IBranch0 _ (_,c,_)) =
    c
  getValueCount (IBranch1 _ (_,c,_) Nothing) =
    c
  getValueCount (IBranch1 _ (_,c1,_) (Just (_,c2,_))) =
    c1+c2
  getValueCount (IBranch2 _ (_,c1,_) (_,c2,_) m i) =
    c1 + c2 + sum (map fst $ Map.elems m) + maybe 0 (\(_,c3,_)->c3) i

instance TreeNode StableTree where
  getDepth (StableTree_I t) = getDepth t
  getDepth (StableTree_C t) = getDepth t

  getValueCount (StableTree_I t) = getValueCount t
  getValueCount (StableTree_C t) = getValueCount t

instance (Eq k, Eq v) => Eq (Tree d c k v) where
  (Bottom lp1 lp2 lnts lt) == (Bottom rp1 rp2 rnts rt) =
    (lp1 == rp1) && (lp2 == rp2) && (lnts == rnts) && (lt == rt)
  (IBottom0 l) == (IBottom0 r) = l == r
  (IBottom1 lp1 lp2 lnts) == (IBottom1 rp1 rp2 rnts) = 
    (lp1 == rp1) && (lp2 == rp2) && (lnts == rnts)

  -- We _could_ check the depth parameter as well, but that's also in the type
  -- signature, so why bother?
  (Branch _ lt1 lt2 lnts lt) == (Branch _ rt1 rt2 rnts rt) =
    (lt1 == rt1) && (lt2 == rt2) && (lnts == rnts) && (lt == rt)
  (IBranch0 _ lt) == (IBranch0 _ rt) = lt == rt
  (IBranch1 _ lt li) == (IBranch1 _ rt ri) = (lt == rt) && (li == ri)
  (IBranch2 _ lt1 lt2 lnts li) == (IBranch2 _ rt1 rt2 rnts ri) =
    (lt1 == rt1) && (lt2 == rt2) && (lnts == rnts) && (li == ri)
  _ == _ = False

instance (Eq k, Eq v) => Eq (StableTree k v) where
  (StableTree_I t1) == (StableTree_I t2) = t1 `equals` t2
  (StableTree_C t1) == (StableTree_C t2) = t1 `equals` t2
  _ == _ = False

equals :: (Eq k, Eq v) => Tree d1 c1 k v -> Tree d2 c2 k v -> Bool
equals l@(Bottom{}) r@(Bottom{})     = l == r
equals l@(IBottom0{}) r@(IBottom0{}) = l == r
equals l@(IBottom1{}) r@(IBottom1{}) = l == r
equals (Branch ld (lk1, lv1, lt1) (lk2, lv2, lt2) lnts (lkt, lvt, ltt))
       (Branch rd (rk1, rv1, rt1) (rk2, rv2, rt2) rnts (rkt, rvt, rtt)) =
  (ld == rd) &&
    (lk1 == rk1) && (lk2 == rk2) && (lkt == rkt) &&
    (lv1 == rv1) && (lv2 == rv2) && (lvt == rvt) &&
    (lt1 `equals` rt1) && (lt2 `equals` rt2) && (ltt `equals` rtt) &&
    (ntEquals lnts rnts)
equals (IBranch0 ld (lk, lv, lt)) (IBranch0 rd (rk, rv, rt)) =
  (ld == rd) && (lk == rk) && (lv == rv) && (lt `equals` rt)
equals (IBranch1 ld (lk, lv, lt) Nothing) (IBranch1 rd (rk, rv, rt) Nothing) =
  (ld == rd) && (lk == rk) && (lv == rv) && (lt `equals` rt)
equals (IBranch1 ld (lk, lv, lt) (Just (lki, lvi, lti)))
       (IBranch1 rd (rk, rv, rt) (Just (rki, rvi, rti))) =
  (ld == rd) && (lk == rk) && (lv == rv) && (lki == rki) && (lvi == rvi) &&
    (lt `equals` rt) && (lti `equals` rti)
equals (IBranch2 ld (lk1, lv1, lt1) (lk2, lv2, lt2) lnts Nothing)
       (IBranch2 rd (rk1, rv1, rt1) (rk2, rv2, rt2) rnts Nothing) =
  (ld == rd) &&
    (lk1 == rk1) && (lk2 == rk2) && (lv1 == rv1) && (lv2 == rv2) &&
    (lt1 `equals` rt1) && (lt2 `equals` rt2) && (ntEquals lnts rnts)
equals (IBranch2 ld (lk1, lv1, lt1) (lk2, lv2, lt2) lnts (Just (lki, lvi, lti)))
       (IBranch2 rd (rk1, rv1, rt1) (rk2, rv2, rt2) rnts (Just (rki, rvi, rti))) =
  (ld == rd) &&
    (lk1 == rk1) && (lk2 == rk2) && (lv1 == rv1) && (lv2 == rv2) &&
    (lki == rki) && (lvi == rvi) && (lti `equals` rti) &&
    (lt1 `equals` rt1) && (lt2 `equals` rt2) && (ntEquals lnts rnts)
equals _ _ = False

ntEquals :: (Eq k, Eq v)
         => Map (Key Nonterminal k) (ValueCount, Tree d1 Complete k v)
         -> Map (Key Nonterminal k) (ValueCount, Tree d2 Complete k v)
         -> Bool
ntEquals lnts rnts =
  (Map.keys lnts == Map.keys rnts) &&
    (map fst (Map.elems lnts) == map fst (Map.elems rnts)) &&
    (all (==True) (zipWith (\l r -> (snd l) `equals` (snd r))
                           (Map.elems lnts)
                           (Map.elems rnts)))

deriving instance (Ord k, Show k, Show v) => Show (StableTree k v)
deriving instance (Ord k, Show k, Show v) => Show (Tree d c k v)

