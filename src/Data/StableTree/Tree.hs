{-# LANGUAGE LambdaCase, OverloadedStrings, GADTs #-}
-- |
-- Module    : Data.StableTree.Tree
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- This is the core implementation of the stable tree. The primary functions
-- exported by this module are 'nextBottom' and 'nextBranch', which gather
-- values or lower-level 'Tree's into 'Tree's of the next level.
--
-- This module is fairly esoteric. "Data.StableTree" or "Data.StableTree.IO"
-- are probably what you actually want to be using.
module Data.StableTree.Tree
( Tree
, Complete
, Incomplete
, nextBottom
, nextBranch
, getKey
, completeKey
, treeContents
, branchContents
, getObjectID
, getDepth
, getValueCount
) where

import Data.StableTree.Key
import Data.StableTree.Types ( ValueCount, Depth )
import Data.StableTree.Fragment ( Fragment(..) )

import qualified Data.Map as Map
import Data.ObjectID      ( ObjectID, calculateSerialize )
import Control.Arrow      ( first, second )
import Data.Map           ( Map )
import Data.List          ( intercalate )
import Data.Serialize     ( Serialize )

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
  Bottom :: ObjectID
         -> (SomeKey k, v)
         -> (SomeKey k, v)
         -> Map (Key Nonterminal k) v
         -> (Key Terminal k, v)
         -> Tree Complete k v

  Branch :: ObjectID
         -> Depth
         -> (SomeKey k, ValueCount, Tree Complete k v)
         -> (SomeKey k, ValueCount, Tree Complete k v)
         -> Map (Key Nonterminal k) (ValueCount, Tree Complete k v)
         -> (Key Terminal k, ValueCount, Tree Complete k v)
         -> Tree Complete k v

  -- Either an empty or a singleton tree
  IBottom0 :: ObjectID
           -> Maybe (SomeKey k, v)
           -> Tree Incomplete k v

  -- Any number of items, but not ending with a terminal key
  IBottom1 :: ObjectID
           -> (SomeKey k, v)
           -> (SomeKey k, v)
           -> Map (Key Nonterminal k) v
           -> Tree Incomplete k v

  -- A strut to lift an incomplete tree to the next level up
  IBranch0 :: ObjectID
           -> Depth
           -> (SomeKey k, ValueCount, Tree Incomplete k v)
           -> Tree Incomplete k v

  -- A joining of a single complete and maybe an incomplete
  IBranch1 :: ObjectID
           -> Depth
           -> (SomeKey k, ValueCount, Tree Complete k v)
           -> Maybe (SomeKey k, ValueCount, Tree Incomplete k v)
           -> Tree Incomplete k v

  -- A branch that doesn't have a terminal, and that might have an IBranch
  IBranch2 :: ObjectID
           -> Depth
           -> (SomeKey k, ValueCount, Tree Complete k v)
           -> (SomeKey k, ValueCount, Tree Complete k v)
           -> Map (Key Nonterminal k) (ValueCount, Tree Complete k v)
           -> Maybe (SomeKey k, ValueCount, Tree Incomplete k v)
           -> Tree Incomplete k v

-- |Wrap up some of a k/v map into a 'Tree'. A 'Right' result gives a complete
-- tree and the map updated to not have the key/values that went into that
-- tree. A 'Left' result gives an incomplete tree that contains everything that
-- the given map contained.
nextBottom :: (Ord k, Serialize k, Serialize v)
           => Map k v
           -> Either (Tree Incomplete k v)
                     (Tree Complete k v, Map k v)
nextBottom values =
  case Map.minViewWithKey values >>= return . second Map.minViewWithKey of
    Just (f1, Just (f2, remain)) ->
      go (first wrap f1) (first wrap f2) Map.empty remain
    partial ->
      -- this is a bit odd, because I couldn't come up with a better way to tie
      -- the type of the Nothing to the type of the Just, so that
      -- iBottom0ObjectID would be satisfied.
      let m = case partial of
                Nothing -> Nothing
                Just ((k,v), Nothing) -> Just (wrap k, v)
                _ ->
                  error "This is just here to satisfy a broken exhaustion check"
          o = iBottom0ObjectID m
          b = IBottom0 o m
      in Left b

  where
  go f1 f2 accum remain =
    case Map.minViewWithKey remain of
      Nothing ->
        Left $ IBottom1 (iBottom1ObjectID f1 f2 accum) f1 f2 accum
      Just ((k, v), remain') ->
        case wrap k of
          SomeKey_N nonterm ->
            go f1 f2 (Map.insert nonterm v accum) remain'
          SomeKey_T term ->
            let oid = bottomObjectID f1 f2 accum (term, v)
            in Right (Bottom oid f1 f2 accum (term, v), remain')

-- |Generate a parent for a k/Tree map. A 'Right' result gives a complete tree
-- and the map updated to not have the key/trees that went into that tree. A
-- 'Left' result gives an incomplete tree that contains everything that the
-- given map contained.
nextBranch :: (Ord k, Serialize k, Serialize v)
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
        Nothing ->
          Left $ IBottom0 (iBottom0ObjectID (nothing branches)) Nothing
        Just (ik, iv) ->
          let tup = (wrap ik, getValueCount iv, iv)
              oid = iBranch0ObjectID depth tup
          in Left $ IBranch0 oid depth tup
    Just ((k,v), Nothing) ->
      let tup = (wrap k, getValueCount v, v)
          may = wrapMKey mIncomplete
          oid = iBranch1ObjectID depth tup may
      in Left $ IBranch1 oid depth tup may
    Just (f1, Just (f2, remain)) ->
      go (wrapKey f1) (wrapKey f2) Map.empty remain

  where
  go f1 f2 accum remain =
    let popd = Map.minViewWithKey remain >>= return . first wrapKey
    in case popd of
      Nothing ->
        let may = wrapMKey mIncomplete
            oid = iBranch2ObjectID depth f1 f2 accum may
        in Left $ IBranch2 oid depth f1 f2 accum may 
      Just ((SomeKey_T term,c,v), remain') ->
        let tup = (term, c, v)
            oid = branchObjectID depth f1 f2 accum tup
        in Right ( Branch oid depth f1 f2 accum tup, remain' )
      Just ((SomeKey_N nonterm,c,v), remain') ->
        go f1 f2 (Map.insert nonterm (c,v) accum) remain'

  wrapKey (k,v) = (wrap k, getValueCount v, v)

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

  nothing :: Map k (Tree Complete k v) -> Maybe (SomeKey k, v)
  nothing _ = Nothing

-- |Get the key of the first entry in this branch. If the branch is empty,
-- returns Nothing.
getKey :: Tree c k v -> Maybe k
getKey (Bottom _ (k,_) _ _ _)       = Just $ unwrap k
getKey (IBottom0 _ Nothing)         = Nothing
getKey (IBottom0 _ (Just (k,_)))    = Just $ unwrap k
getKey (IBottom1 _ (k,_) _ _)       = Just $ unwrap k
getKey (Branch _ _ (k,_,_) _ _ _)   = Just $ unwrap k
getKey (IBranch0 _ _ (k,_,_))       = Just $ unwrap k
getKey (IBranch1 _ _ (k,_,_) _)     = Just $ unwrap k
getKey (IBranch2 _ _ (k,_,_) _ _ _) = Just $ unwrap k

-- |Get the key of the fist entry in this complete branch. This function is
-- total.
completeKey :: Tree Complete k v -> k
completeKey (Bottom _ (k,_) _ _ _)     = unwrap k
completeKey (Branch _ _ (k,_,_) _ _ _) = unwrap k

-- |Convert an entire Tree into a k/v map.
treeContents :: Ord k => Tree c k v -> Map k v
treeContents t =
  case branchContents t of
    Left ( completes, Nothing) ->
      Map.unions $ map (treeContents . snd) $ Map.elems completes
    Left ( completes, Just (_k, _c, iv)) ->
      Map.unions $ treeContents iv:map (treeContents . snd) (Map.elems completes)
    Right x -> x

-- |Get the ObjectID of a tree node
getObjectID :: Tree c k v -> ObjectID
getObjectID (Bottom o _ _ _ _)     = o
getObjectID (Branch o _ _ _ _ _)   = o
getObjectID (IBottom0 o _)         = o
getObjectID (IBottom1 o _ _ _)     = o
getObjectID (IBranch0 o _ _)       = o
getObjectID (IBranch1 o _ _ _)     = o
getObjectID (IBranch2 o _ _ _ _ _) = o

-- |Get the number of levels of branches that live below this one
getDepth :: Tree c k v -> Depth
getDepth (Bottom _ _ _ _ _)     = 0
getDepth (Branch _ d _ _ _ _)   = d
getDepth (IBottom0 _ _)         = 0
getDepth (IBottom1 _ _ _ _)     = 0
getDepth (IBranch0 _ d _)       = d
getDepth (IBranch1 _ d _ _)     = d
getDepth (IBranch2 _ d _ _ _ _) = d

-- |Get the number of actual values that live below this branch
getValueCount :: Tree c k v -> ValueCount
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

-- |Non-recursive function to simply get the immediate children of the given
-- branch. This will either give the key/value map of a Bottom, or the key/tree
-- map of a non-bottom branch.
branchContents :: Ord k
               => Tree c k v
               -> Either ( Map k (ValueCount, Tree Complete k v)
                         , Maybe (k, ValueCount, Tree Incomplete k v))
                         ( Map k v )
branchContents (Bottom _ (k1,v1) (k2,v2) terms (kt,vt)) =
  let terms' = Map.mapKeys fromKey terms
      conts  = Map.insert (unwrap k1) v1
             $ Map.insert (unwrap k2) v2
             $ Map.insert (fromKey kt) vt
             terms'
  in Right conts
branchContents (Branch _ _d (k1,c1,v1) (k2,c2,v2) terms (kt,ct,vt)) =
  let terms' = Map.mapKeys fromKey terms
      conts  = Map.insert (unwrap k1) (c1,v1)
             $ Map.insert (unwrap k2) (c2,v2)
             $ Map.insert (fromKey kt) (ct,vt)
             terms'
  in Left (conts, Nothing)
branchContents (IBottom0 _ Nothing) =
  Right Map.empty
branchContents (IBottom0 _ (Just (k,v))) =
  Right $ Map.singleton (unwrap k) v
branchContents (IBottom1 _ (k1,v1) (k2,v2) terms) =
  let terms' = Map.mapKeys fromKey terms
      conts  = Map.insert (unwrap k1) v1
             $ Map.insert (unwrap k2) v2
             terms'
  in Right conts
branchContents (IBranch0 _ _d (ik,ic,iv)) =
  Left (Map.empty, Just (unwrap ik, ic, iv))
branchContents (IBranch1 _ _d (k1,c1,v1) mIncomplete) =
  Left ( Map.singleton (unwrap k1) (c1,v1)
       , mIncomplete >>= (\(k,c,v) -> return (unwrap k,c,v)))
branchContents (IBranch2 _ _d (k1,c1,v1) (k2,c2,v2) terms mIncomplete) =
  let terms' = Map.mapKeys fromKey terms
      conts  = Map.insert (unwrap k1) (c1,v1)
             $ Map.insert (unwrap k2) (c2,v2)
             terms'
  in Left (conts, mIncomplete >>= \(k,c,v) -> return (unwrap k, c, v))

instance (Ord k, Show k, Show v) => Show (Tree c k v) where
  show t@(Bottom _ _ _ _ _)     = branchShow "Bottom" t
  show t@(Branch _ _ _ _ _ _)   = branchShow "Branch" t
  show t@(IBottom0 _ _)         = branchShow "IBottom" t
  show t@(IBottom1 _ _ _ _)     = branchShow "IBottom" t
  show t@(IBranch0 _ _ _)       = branchShow "IBranch" t
  show t@(IBranch1 _ _ _ _)     = branchShow "IBranch" t
  show t@(IBranch2 _ _ _ _ _ _) = branchShow "IBranch" t

branchShow :: (Ord k, Show k, Show v) => String -> Tree c k v -> String
branchShow header t =
  case branchContents t of
    Left (ts, Nothing) ->
      let strs = [show k ++ " => " ++ show v | (k, v) <- Map.toAscList ts]
          str  = intercalate ", " strs
      in header ++ "(" ++ show (getDepth t) ++ ")<" ++ str ++ ">"
    Left (ts, Just (ik, _ic, iv)) ->
      let strs = [ show k ++ " => " ++ show v | (k, v) <- Map.toAscList ts
                 ] ++ [show ik ++ " => " ++ show iv]
          str  = intercalate ", " strs
      in header ++ "(" ++ show (getDepth t) ++ ")<" ++ str ++ ">"
    Right vals ->
      let strs = [ show k ++ " => " ++ show v | (k, v) <- Map.toAscList vals ]
          str  = intercalate ", " strs
      in header ++ "(" ++ show (getDepth t) ++ ")<" ++ str ++ ">"

bottomObjectID :: (Ord k, Serialize k, Serialize v)
               => (SomeKey k, v)
               -> (SomeKey k, v)
               -> Map (Key Nonterminal k) v
               -> (Key Terminal k, v)
               -> ObjectID
bottomObjectID (sk1, v1) (sk2, v2) ntmap (tk3, v3) =
  let m = Map.insert (unwrap sk1) v1
        $ Map.insert (unwrap sk2) v2
        $ Map.insert (fromKey tk3) v3
        $ Map.mapKeys fromKey ntmap
  in calculateSerialize $ FragmentBottom m

branchObjectID :: (Ord k, Serialize k, Serialize v)
               => Depth
               -> (SomeKey k, ValueCount, Tree Complete k v)
               -> (SomeKey k, ValueCount, Tree Complete k v)
               -> Map (Key Nonterminal k) (ValueCount, Tree Complete k v)
               -> (Key Terminal k, ValueCount, Tree Complete k v)
               -> ObjectID
branchObjectID d (sk1, c1, t1) (sk2, c2, t2) ntmap (tk3, c3, t3) =
  let m = Map.insert (unwrap sk1) (c1,getObjectID t1)
        $ Map.insert (unwrap sk2) (c2,getObjectID t2)
        $ Map.insert (fromKey tk3) (c3,getObjectID t3)
        $ Map.map (second getObjectID)
        $ Map.mapKeys fromKey ntmap
      b = FragmentBranch d m
      _ = witness t1 b
  in calculateSerialize b

iBottom0ObjectID :: (Ord k, Serialize k, Serialize v)
                 => Maybe (SomeKey k, v)
                 -> ObjectID
iBottom0ObjectID mkv =
  let m = Map.empty
      -- This funny dance ties the type of 'm' to the types of 'k' and 'v', so
      -- our empty fragment bottom can type check
      f = case mkv of
            Nothing -> FragmentBottom m
            Just (sk, v) -> FragmentBottom $ Map.insert (unwrap sk) v m
  in calculateSerialize f

iBottom1ObjectID :: (Ord k, Serialize k, Serialize v)
                 => (SomeKey k, v)
                 -> (SomeKey k, v)
                 -> Map (Key Nonterminal k) v
                 -> ObjectID
iBottom1ObjectID (sk1, v1) (sk2, v2) ntmap =
  let m = Map.insert (unwrap sk1) v1
        $ Map.insert (unwrap sk2) v2
        $ Map.mapKeys fromKey ntmap
      b = FragmentBottom m
  in calculateSerialize b

iBranch0ObjectID :: (Ord k, Serialize k, Serialize v)
                 => Depth
                 -> (SomeKey k, ValueCount, Tree Incomplete k v)
                 -> ObjectID
iBranch0ObjectID d (sk,c,t) =
  let m = Map.singleton (unwrap sk) (c, getObjectID t)
      b = FragmentBranch d m
      _ = witness t b
  in calculateSerialize b

iBranch1ObjectID :: (Ord k, Serialize k, Serialize v)
                 => Depth
                 -> (SomeKey k, ValueCount, Tree Complete k v)
                 -> Maybe (SomeKey k, ValueCount, Tree Incomplete k v)
                 -> ObjectID
iBranch1ObjectID d (sk, c, t) minc =
  let m = Map.singleton (unwrap sk) (c, getObjectID t)
      m' = case minc of
            Nothing -> m
            Just (sk', c', t') ->
              Map.insert (unwrap sk') (c', getObjectID t') m
      b = FragmentBranch d m'
      _ = witness t b
  in calculateSerialize b

iBranch2ObjectID :: (Ord k, Serialize k, Serialize v)
                 => Depth
                 -> (SomeKey k, ValueCount, Tree Complete k v)
                 -> (SomeKey k, ValueCount, Tree Complete k v)
                 -> Map (Key Nonterminal k) (ValueCount, Tree Complete k v)
                 -> Maybe (SomeKey k, ValueCount, Tree Incomplete k v)
                 -> ObjectID
iBranch2ObjectID d (sk1, c1, t1) (sk2, c2, t2) ntmap minc =
  let m = Map.insert (unwrap sk1) (c1, getObjectID t1)
        $ Map.insert (unwrap sk2) (c2, getObjectID t2)
        $ Map.mapKeys fromKey
        $ Map.map (second getObjectID) ntmap
      m' = case minc of
            Nothing -> m
            Just (sk3, c3, t3) ->
              Map.insert (unwrap sk3) (c3, getObjectID t3) m
      b = FragmentBranch d m'
      _ = witness t1 b
  in calculateSerialize b

-- 'FragmentBranch' doesn't rely at all on the 'v' part of the given trees,
-- as it only maps keys to ObjectIDs. This witness ties the fragment to the
-- tree so the type checker can guarantee the 'Serialize v' instance that
-- allows us to calculate the ObjectID.
witness :: Tree c k v -> Fragment k v -> ()
witness _ _ = ()
