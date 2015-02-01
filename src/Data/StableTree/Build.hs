{-# LANGUAGE LambdaCase, OverloadedStrings, GADTs, ExistentialQuantification #-}
-- |
-- Module    : Data.StableTree.Build
-- Copyright : Jeremy Groven
-- License   : BSD3
--
-- This is the core implementation of the stable tree. The primary functions
-- exported by this module are 'nextBottom' and 'nextBranch', which gather
-- values or lower-level 'Tree's into 'Tree's of the next level.
--
-- This module is fairly esoteric. "Data.StableTree" is probably what you
-- actually want to be using.
module Data.StableTree.Build
( fromMap
, empty
, append
, concat
, consume
, consumeMap
, consumeBranches
, consumeBranches'
, nextBottom
, NextBranch(..)
, nextBranch
, merge
) where

import qualified Data.StableTree.Key as Key
import qualified Data.StableTree.Properties as Properties
import Data.StableTree.Key   ( SomeKey(..), fromKey, unwrap )
import Data.StableTree.Types

import qualified Data.Map as Map
import Control.Arrow  ( first, second )
import Data.Map       ( Map )
import Data.Maybe     ( maybeToList )
import Data.List      ( sortBy )
import Data.Ord       ( comparing )
import Data.Serialize ( Serialize )

import Prelude hiding ( concat )

-- |Convert a simple key/value map into a StableTree
fromMap :: (Ord k, Serialize k, Serialize v) => Map k v -> StableTree k v
fromMap = (uncurry consume) . consumeMap

-- |Create a new empty StableTree
empty :: (Ord k, Serialize k, Serialize v) => StableTree k v
empty = case consumeMap Map.empty of
          ([], Just inc) -> StableTree_I inc
          ([complete], Nothing) -> StableTree_C complete
          _ -> error "an empty tree _does not_ have more than one item"

-- |Smash two StableTree instances into a single one
append :: (Ord k, Serialize k, Serialize v)
       => StableTree k v -> StableTree k v -> StableTree k v
append l r = concat [l, r]

-- |Smash a whole bunch of StableTree instances into a single one
concat :: (Ord k, Serialize k, Serialize v)
       => [StableTree k v] -> StableTree k v
concat = go [] []
  where
  go :: (Ord k, Serialize k, Serialize v)
     => [Tree Z Complete k v] -> [Tree Z Incomplete k v] -> [StableTree k v]
     -> StableTree k v
  go completes incompletes [] = concat' completes incompletes
  go cs is (StableTree_C c:rest) =
    case c of
      Bottom _ _ _ _ _   -> go (c:cs) is rest
      Branch _ _ _ _ _ _ -> branch c cs is rest
  go cs is (StableTree_I i:rest) =
    case i of
      IBottom0 _ _         -> go cs (i:is) rest
      IBottom1 _ _ _ _     -> go cs (i:is) rest
      IBranch0 _ _ _       -> branch i cs is rest
      IBranch1 _ _ _ _     -> branch i cs is rest
      IBranch2 _ _ _ _ _ _ -> branch i cs is rest

  branch :: (Ord k, Serialize k, Serialize v)
         => Tree (S d) c k v
         -> [Tree Z Complete k v]
         -> [Tree Z Incomplete k v]
         -> [StableTree k v]
         -> StableTree k v
  branch i cs is rest =
    let (children, minc) = Properties.branchChildren i
        child'           = map (StableTree_C . snd) $ Map.elems children
        inc'             = map (\(_, _, t) -> StableTree_I t)
                               (maybeToList minc)
    in go cs is (inc' ++ child' ++ rest)

-- |Helper function to convert a complete bunch of Tree instances (of the same
-- depth) into a single StableTree.
consume :: (Ord k, Serialize k, Serialize v)
        => [Tree d Complete k v]
        -> Maybe (Tree d Incomplete k v)
        -> StableTree k v
consume [] Nothing = empty
consume [c] Nothing = prune $ StableTree_C c
consume [] (Just i) = prune $ StableTree_I i
consume cs minc =
  (uncurry consume) (consumeBranches' cs minc)

-- |Helper function to reduce trees to their minimum height by removing root
-- branches that only have one child.
prune :: Ord k => StableTree k v -> StableTree k v
prune st =
  case Properties.stableChildren st of
    Left _ -> st
    Right m ->
      -- This may be too wasteful; we'll find out.
      case Map.elems m of
        [(_,c)] -> prune c
        _ -> st

-- |Convert a single key/value map into Tree bottom (zero-depth) instances. The
-- resulting list of Tree instances will never be overlapping, and will be
-- sorted such that each Tree's highest key is lower than the next Tree's
-- lowest key. This is not guaranteed by types because i don't think that can
-- be done in Haskell.
consumeMap :: (Ord k, Serialize k, Serialize v)
           => Map k v
           -> ([Tree Z Complete k v], Maybe (Tree Z Incomplete k v))
consumeMap = go []
  where
  go accum remain =
    case nextBottom remain of
      Left inc ->
        (reverse accum, Just inc)
      Right (comp, remain') ->
        if Map.null remain'
          then (reverse (comp:accum), Nothing)
          else go (comp:accum) remain'

-- |Given a mapping from each Tree's first key to that Tree, (and a final
-- incomplete Tree if desired), this will build the next level of Tree
-- instances. As with consumeMap, the resulting list of Tree instances will be
-- non-overlapping and ordered such that each Tree's highest key is smaller
-- than the next Tree's lowest key.
consumeBranches :: (Ord k, Serialize k, Serialize v)
                => Map k (Tree d Complete k v)
                -> Maybe (k, Tree d Incomplete k v)
                -> ([Tree (S d) Complete k v], Maybe (Tree (S d) Incomplete k v))
consumeBranches = go []
  where
  go accum remain minc =
    case nextBranch remain minc of
      Empty ->
        (reverse accum, Nothing) -- I think accum is probably [] here...
      Final inc ->
        (reverse accum, Just inc)
      More comp remain' ->
        go (comp:accum) remain' minc

-- |Given a simple listing of complete Trees and maybe an incomplete one, this
-- will build the next level ot Trees. This just builds a map and calls the
-- previous 'consumeBranches' function, but it's a convenient function to have.
consumeBranches' :: (Ord k, Serialize k, Serialize v)
                 => [Tree d Complete k v]
                 -> Maybe (Tree d Incomplete k v)
                 -> ([Tree (S d) Complete k v], Maybe (Tree (S d) Incomplete k v))
consumeBranches' completes mincomplete =
  let ctree = Map.fromList [(Properties.completeKey c, c) | c <- completes]
      mpair = case mincomplete of
                Nothing -> Nothing
                Just inc ->
                  case Properties.getKey inc of
                    Nothing -> Nothing
                    Just k -> Just (k, inc)
  in consumeBranches ctree mpair

-- |Wrap up some of a k/v map into a 'Tree'. A 'Right' result gives a complete
-- tree and the map updated to not have the key/values that went into that
-- tree. A 'Left' result gives an incomplete tree that contains everything that
-- the given map contained.
nextBottom :: (Ord k, Serialize k, Serialize v)
           => Map k v
           -> Either (Tree Z Incomplete k v)
                     (Tree Z Complete k v, Map k v)
nextBottom values =
  case Map.minViewWithKey values >>= return . second Map.minViewWithKey of
    Just (f1, Just (f2, remain)) ->
      go (first Key.wrap f1) (first Key.wrap f2) Map.empty remain
    partial ->
      -- this is a bit odd, because I couldn't come up with a better way to tie
      -- the type of the Nothing to the type of the Just, so that
      -- iBottom0ObjectID would be satisfied.
      let m = case partial of
                Nothing -> Nothing
                Just ((k,v), Nothing) -> Just (Key.wrap k, v)
                _ ->
                  error "This is just here to satisfy a broken exhaustion check"
          b = mkIBottom0 m
      in Left b

  where
  go f1 f2 accum remain =
    case Map.minViewWithKey remain of
      Nothing ->
        Left $ mkIBottom1 f1 f2 accum
      Just ((k, v), remain') ->
        case Key.wrap k of
          SomeKey_N nonterm ->
            go f1 f2 (Map.insert nonterm v accum) remain'
          SomeKey_T term ->
            Right (mkBottom f1 f2 accum (term, v), remain')

-- | Result of the 'nextBranch' function; values are described below.
data NextBranch d k v
  = Empty
  | Final (Tree (S d) Incomplete k v)
  | More  (Tree (S d) Complete k v) (Map k (Tree d Complete k v))

-- |Generate a parent for a k/Tree map. An 'Empty' result means that the
-- function was called with an empty Map and 'Nothing' for an incomplete. A
-- 'Final' result means that an incomplete Tree was build and there is no more
-- work to be done. A 'More' result means that a complete Tree was built, and
-- there is (possibly) more work to do.
nextBranch :: (Ord k, Serialize k, Serialize v)
           => Map k (Tree d Complete k v)
           -> Maybe (k, Tree d Incomplete k v)
           -> NextBranch d k v
nextBranch branches mIncomplete =
  let freebies = Map.minViewWithKey branches
                 >>= return . second Map.minViewWithKey
  in case freebies of
    Nothing -> 
      case mIncomplete of
        Nothing ->
          Empty
        Just (ik, iv) ->
          let tup = (Key.wrap ik, getValueCount iv, iv)
              b   = mkIBranch0 depth tup
          in Final b
    Just ((k,v), Nothing) ->
      let tup = (Key.wrap k, getValueCount v, v)
          may = wrapMKey mIncomplete
      in Final $ mkIBranch1 depth tup may
    Just (f1, Just (f2, remain)) ->
      go (wrapKey f1) (wrapKey f2) Map.empty remain

  where
  go f1 f2 accum remain =
    let popd = Map.minViewWithKey remain >>= return . first wrapKey
    in case popd of
      Nothing ->
        let may = wrapMKey mIncomplete
        in Final $ mkIBranch2 depth f1 f2 accum may 
      Just ((SomeKey_T term,c,v), remain') ->
        let tup = (term, c, v)
        in More (mkBranch depth f1 f2 accum tup) remain'
      Just ((SomeKey_N nonterm,c,v), remain') ->
        go f1 f2 (Map.insert nonterm (c,v) accum) remain'

  wrapKey (k,v) = (Key.wrap k, getValueCount v, v)

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

-- |Tree mutation functions (insert, delete) will generally wind up with a
-- bunch of Trees that come before the key that was to be changed, and then the
-- result of updating the relevant Tree, and then a bunch of Trees (and maybe
-- an incomplete Tree) that come after it. Merge can splice this result back
-- into a correctly ordered, non-overlapping list of complete Trees and maybe a
-- final incomplete one.
merge :: (Ord k, Serialize k, Serialize v)
      => [Tree d Complete k v]
      -> Maybe (Tree d Incomplete k v)
      -> [Tree d Complete k v]
      -> Maybe (Tree d Incomplete k v)
      -> ([Tree d Complete k v], Maybe (Tree d Incomplete k v))
merge before Nothing after minc =
  (before ++ after, minc)
merge before minc [] Nothing =
  (before, minc)
merge before (Just left) [] (Just right) =
  case left of
    (IBottom0 _ _)         -> bottom before left right
    (IBottom1 _ _ _ _)     -> bottom before left right
    (IBranch0 _ _ _)       -> branch before left right
    (IBranch1 _ _ _ _)     -> branch before left right
    (IBranch2 _ _ _ _ _ _) -> branch before left right

  where
  bottom b l r =
    let lc            = Properties.bottomChildren l
        rc            = Properties.bottomChildren r
        (after, minc) = consumeMap (Map.union lc rc)
    in (b ++ after, minc)

  branch :: (Ord k, Serialize k, Serialize v)
           => [Tree (S d) Complete k v]
           -> Tree (S d) Incomplete k v
           -> Tree (S d) Incomplete k v
           -> ([Tree (S d) Complete k v], Maybe (Tree (S d) Incomplete k v))
  branch b l r =
    let (c1, i1)      = Properties.branchChildren l
        c1'           = map snd $ Map.elems c1
        i1'           = fmap (\(_,_,x) -> x) i1
        (c2, i2)      = Properties.branchChildren r
        c2'           = map snd $ Map.elems c2
        i2'           = fmap (\(_,_,x) -> x) i2
        (lcomp, linc) = merge c1' i1' c2' i2'
        lcomp'        = Map.fromList [(Properties.completeKey i,i)|i<-lcomp]
        linc'         = case linc of
                          Nothing -> Nothing
                          Just i ->
                            case Properties.getKey i of
                              Nothing -> Nothing
                              Just k  -> Just (k,i)
        (after, minc) = consumeBranches lcomp' linc'
    in (b ++ after, minc)

merge before (Just inc) (after:rest) minc =
  case inc of
    (IBottom0 _ _) -> bottom before inc after rest minc
    (IBottom1 _ _ _ _) -> bottom before inc after rest minc
    (IBranch0 _ _ _) -> branch before inc after rest minc
    (IBranch1 _ _ _ _) -> branch before inc after rest minc
    (IBranch2 _ _ _ _ _ _) -> branch before inc after rest minc
  where
  bottom :: (Ord k, Serialize k, Serialize v)
           => [Tree Z Complete k v]
           -> Tree Z Incomplete k v
           -> Tree Z Complete k v
           -> [Tree Z Complete k v]
           -> Maybe (Tree Z Incomplete k v)
           -> ([Tree Z Complete k v], Maybe (Tree Z Incomplete k v))
  bottom b i a r m =
    let ic = Properties.bottomChildren i
        ac = Properties.bottomChildren a
    in case consumeMap (Map.union ic ac) of
        (comp, Nothing) -> (b++comp++r, m)
        (comp, justinc) -> merge (b++comp) justinc r m

  branch :: (Ord k, Serialize k, Serialize v)
           => [Tree (S d) Complete k v]
           -> Tree (S d) Incomplete k v
           -> Tree (S d) Complete k v
           -> [Tree (S d) Complete k v]
           -> Maybe (Tree (S d) Incomplete k v)
           -> ([Tree (S d) Complete k v], Maybe (Tree (S d) Incomplete k v))
  branch b i a r m =
    let (ci, ii)             = Properties.branchChildren i
        ci'                  = map snd $ Map.elems ci
        ii'                  = fmap (\(_,_,x) -> x) ii
        (ca, ia)             = Properties.branchChildren a
        ca'                  = map snd $ Map.elems ca
        ia'                  = fmap (\(_,_,x) -> x) ia
        (low_comp, low_minc) = merge ci' ii' ca' ia'
        lcomp'               = Map.fromList [ (Properties.completeKey lc, lc)
                                            | lc <- low_comp]
        linc'                = case low_minc of
                                 Nothing -> Nothing
                                 Just low_inc ->
                                   case Properties.getKey low_inc of
                                     Nothing -> Nothing
                                     Just k  -> Just (k,low_inc)
        (newcomp, newminc)   = consumeBranches lcomp' linc'
    in merge (b ++ newcomp) newminc r m

concat' :: (Ord k, Serialize k, Serialize v)
        => [Tree Z Complete k v]
        -> [Tree Z Incomplete k v]
        -> StableTree k v
concat' completes incompletes =
  let c_triplets = [ (Properties.completeKey c, completeEnd c, Right c)
                   | c <- completes ]
      i_triplets = sort' [ (k, e, Left i) | (Just k, Just e, i) <- 
                           [ (Properties.getKey i, getEnd i, i)
                           | i <- incompletes ] ]
      sorted     = sort' $ c_triplets ++ i_triplets
  in go [] sorted

  where
  go accum [] =
    consume accum Nothing
  go accum [(_, _, Left i)] =
    consume accum (Just i)
  go accum (triple:triples) =
    let (cont, rest) = eatList Map.empty triple triples
    in case cont of
        (cs, Nothing) ->
          go (accum ++ cs) rest
        (cs, Just incomplete) ->
          case (Properties.getKey incomplete, getEnd incomplete) of
            (Just ibegin, Just iend) ->
              go (accum ++ cs) ((ibegin, iend, Left incomplete):rest)
            _ ->
              go (accum ++ cs) rest
  
  eatList kvmap (_, _, Left i) [] | Map.null kvmap =
    (([], Just i), [])
  eatList kvmap (_, _, Right c) [] | Map.null kvmap =
    (([c], Nothing), [])
  eatList kvmap (_, _, x) [] =
    let cont = case x of
                Left i -> Properties.bottomChildren i
                Right c -> Properties.bottomChildren c
        both = Map.union kvmap cont
    in ( consumeMap both, [] )
  eatList kvmap (_, lhi, Right c) rest@((rlow, _, _):_) | Map.null kvmap && lhi < rlow =
    (([c], Nothing), rest)
  eatList kvmap (_, lhi, Right c) rest@((rlow, _, _):_) | lhi < rlow =
    let cont = Properties.bottomChildren c
        both = Map.union kvmap cont
        nxt  = consumeMap both
    in ( nxt, rest )
  eatList kvmap (_, _, x) (nxt:rest) =
    let cont = case x of
                Left l  -> Properties.bottomChildren l
                Right r -> Properties.bottomChildren r
        both = Map.union kvmap cont
    in eatList both nxt rest

  sort' = sortBy (comparing (\(a,b,_) -> (a,b)))

  completeEnd :: Tree Z Complete k v -> k
  completeEnd (Bottom _ _ _ _ (tk, _tv)) = fromKey tk

  getEnd :: Tree Z Incomplete k v -> Maybe k
  getEnd (IBottom0 _ Nothing) =
    Nothing
  getEnd (IBottom0 _ (Just (sk, _v))) =
    Just $ unwrap sk
  getEnd (IBottom1 _ _ (sk, _v) ntmap) =
    case Map.toDescList ntmap of
      []       -> Just $ unwrap sk
      (k,_v):_ -> Just $ fromKey k

  _1 (x, _, _) = x
  _2 (_, x, _) = x
  _3 (_, _, x) = x
