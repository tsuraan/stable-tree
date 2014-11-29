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
-- This module is fairly esoteric. "Data.StableTree" or "Data.StableTree.IO"
-- are probably what you actually want to be using.
module Data.StableTree.Build
( NextBranch(..)
, consumeMap
, nextBottom
, consume
, consumeBranches
, consumeBranches'
, nextBranch
, merge
, empty
, append
, concat
) where

import qualified Data.StableTree.Key as Key
import Data.StableTree.Key      ( SomeKey(..), fromKey, unwrap )
import Data.StableTree.Types
import Data.StableTree.Properties

import qualified Data.Map as Map
import Control.Arrow  ( first, second )
import Data.Map       ( Map )
import Data.Maybe     ( maybeToList )
import Data.List      ( sortBy )
import Data.Ord       ( comparing )
import Data.Serialize ( Serialize )

import Prelude hiding ( concat )

consume :: (Ord k, Serialize k, Serialize v)
        => [Tree d Complete k v]
        -> Maybe (Tree d Incomplete k v)
        -> StableTree k v
consume [] Nothing = empty
consume [c] Nothing = StableTree_C c
consume [] (Just i) = StableTree_I i
consume cs minc =
  (uncurry consume) (consumeBranches' cs minc)

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

consumeBranches' :: (Ord k, Serialize k, Serialize v)
                 => [Tree d Complete k v]
                 -> Maybe (Tree d Incomplete k v)
                 -> ([Tree (S d) Complete k v], Maybe (Tree (S d) Incomplete k v))
consumeBranches' completes mincomplete =
  let ctree = Map.fromList [(completeKey c, c) | c <- completes]
      mpair = case mincomplete of
                Nothing -> Nothing
                Just inc ->
                  case getKey inc of
                    Nothing -> Nothing
                    Just k -> Just (k, inc)
  in consumeBranches ctree mpair

data NextBranch d k v
  = Empty
  | Final (Tree (S d) Incomplete k v)
  | More  (Tree (S d) Complete k v) (Map k (Tree d Complete k v))

-- |Generate a parent for a k/Tree map. A 'Right' result gives a complete tree
-- and the map updated to not have the key/trees that went into that tree. A
-- 'Left' result gives an incomplete tree that contains everything that the
-- given map contained.
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
    let lc            = bottomChildren l
        rc            = bottomChildren r
        (after, minc) = consumeMap (Map.union lc rc)
    in (b ++ after, minc)

  branch :: (Ord k, Serialize k, Serialize v)
           => [Tree (S d) Complete k v]
           -> Tree (S d) Incomplete k v
           -> Tree (S d) Incomplete k v
           -> ([Tree (S d) Complete k v], Maybe (Tree (S d) Incomplete k v))
  branch b l r =
    let (c1, i1)      = branchChildren l
        c1'           = map snd $ Map.elems c1
        i1'           = fmap (\(_,_,x) -> x) i1
        (c2, i2)      = branchChildren r
        c2'           = map snd $ Map.elems c2
        i2'           = fmap (\(_,_,x) -> x) i2
        (lcomp, linc) = merge c1' i1' c2' i2'
        lcomp'        = Map.fromList [(completeKey i,i)|i<-lcomp]
        linc'         = case linc of
                          Nothing -> Nothing
                          Just i ->
                            case getKey i of
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
    let ic = bottomChildren i
        ac = bottomChildren a
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
    let (ci, ii)             = branchChildren i
        ci'                  = map snd $ Map.elems ci
        ii'                  = fmap (\(_,_,x) -> x) ii
        (ca, ia)             = branchChildren a
        ca'                  = map snd $ Map.elems ca
        ia'                  = fmap (\(_,_,x) -> x) ia
        (low_comp, low_minc) = merge ci' ii' ca' ia'
        lcomp'               = Map.fromList [ (completeKey lc, lc)
                                            | lc <- low_comp]
        linc'                = case low_minc of
                                 Nothing -> Nothing
                                 Just low_inc ->
                                   case getKey low_inc of
                                     Nothing -> Nothing
                                     Just k  -> Just (k,low_inc)
        (newcomp, newminc)   = consumeBranches lcomp' linc'
    in merge (b ++ newcomp) newminc r m

empty :: (Ord k, Serialize k, Serialize v) => StableTree k v
empty = case consumeMap Map.empty of
          ([], Just inc) -> StableTree_I inc
          ([complete], Nothing) -> StableTree_C complete
          _ -> error "an empty tree _does not_ have more than one item"

append :: (Ord k, Serialize k, Serialize v)
       => StableTree k v -> StableTree k v -> StableTree k v
append l r = concat [l, r]

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
    let (children, minc) = branchChildren i
        child'           = map (StableTree_C . snd) $ Map.elems children
        inc'             = map (\(_, _, t) -> StableTree_I t)
                               (maybeToList minc)
    in go cs is (inc' ++ child' ++ rest)

concat' :: (Ord k, Serialize k, Serialize v)
        => [Tree Z Complete k v]
        -> [Tree Z Incomplete k v]
        -> StableTree k v
concat' completes incompletes =
  let c_triplets = [ (completeKey c, completeEnd c, Right c) | c <- completes ]
      i_triplets = sort' [ (k, e, Left i) | (Just k, Just e, i) <- 
                           [ (getKey i, getEnd i, i) | i <- incompletes ] ]
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
          case (getKey incomplete, getEnd incomplete) of
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
                Left i -> bottomChildren i
                Right c -> bottomChildren c
        both = Map.union kvmap cont
    in ( consumeMap both, [] )
  eatList kvmap (_, lhi, Right c) rest@((rlow, _, _):_) | Map.null kvmap && lhi < rlow =
    (([c], Nothing), rest)
  eatList kvmap (_, lhi, Right c) rest@((rlow, _, _):_) | lhi < rlow =
    let cont = bottomChildren c
        both = Map.union kvmap cont
        nxt  = consumeMap both
    in ( nxt, rest )
  eatList kvmap (_, _, x) (nxt:rest) =
    let cont = case x of
                Left l  -> bottomChildren l
                Right r -> bottomChildren r
        both = Map.union kvmap cont
    in eatList both nxt rest

  {-
  go accum [] [(_, _, i)] =
    consume accum $ Just i
  go accum [] is =
    let entire     = Map.unions $ map (bottomChildren . _3) is
        (cs, minc) = consumeMap entire
    in consume (accum ++ cs) minc
    -}

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
