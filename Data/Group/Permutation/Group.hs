{-# LANGUAGE BangPatterns, RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Group.Permutation.Group (
  PermGroup, deg, generators, cosetTables, order, permutationGroup, member, subgroup, exhaustive, isSubgroup,
  orbits) where

import Control.Exception.Base
import Control.Monad.ST
import Debug.Trace

import qualified Control.Monad as M
import qualified Data.List as L

import Data.Group.Permutation.Permutation
import Data.Vector (Vector, create)
import qualified Data.Vector as V
import Data.Vector.Mutable
import qualified Data.Vector.Mutable as MV
import qualified Data.IntSet as IS

import Prelude hiding (read, filter, (*))

data PermGroup = Group {
  deg :: !Int,
  order :: Int,
  generators :: Vector Perm,
  cosetTables :: Vector (Vector Perm),
  orbits :: Vector IS.IntSet}

instance Eq PermGroup where
  g == h = order g == order h && isSubgroup g h

instance Show PermGroup where
  show Group{generators} = "<" ++ L.intercalate ", " (L.map show $ V.toList generators) ++ ">"

isSubgroup :: PermGroup -> PermGroup -> Bool
g `isSubgroup` h = deg g == deg h && order g <= order h && V.all (`member` h) (generators g)

mkGroup :: Int -> Maybe [Perm] -> Vector (Vector Perm) -> PermGroup
mkGroup deg gens cosetTables = let
  generators = V.fromList $ let
    gens' = nubPerms (L.concatMap V.toList (V.toList cosetTables))
    in case gens of
      Just gens
	| L.length gens < L.length gens'
		-> gens
      _		-> gens'
  orbits = constructOrbits deg (V.toList generators)
  order = V.product (V.map V.length cosetTables)
  in Group{..}

permutationGroup :: Int -> [Perm] -> PermGroup
permutationGroup !deg gens = assert (L.all (\ g -> degree g == deg) gens) $ let
  cosetTables = V.map V.fromList $ buildTables deg gens
  in mkGroup deg (Just gens) cosetTables

member :: Perm -> PermGroup -> Bool
member alpha Group{cosetTables, deg} = assert (degree alpha == deg) $ member_loop alpha 0 where
  member_loop !alpha !i
    | i >= V.length cosetTables = True
    | otherwise = case V.find (`fixes` i) (V.map (\ gamma -> inverse gamma * alpha) (V.unsafeIndex cosetTables i)) of
	Just alpha' -> member_loop alpha' (i+1)
	Nothing -> False

subgroup :: (Perm -> Bool) -> PermGroup -> PermGroup
subgroup inH g@Group{deg, generators} = let
  subgroupTables = V.map V.fromList $ create $ do
    table <- MV.replicate deg [identity deg]
    let filters = V.cons inH $ V.generate (deg-1) (flip fixes)
    let go_build (alpha:queue) = do
	  result <- filter deg table filters alpha
	  go_build (queue ++ result)
	go_build [] = return ()
    go_build (V.toList generators)
    return (MV.unsafeTail table)
  in mkGroup deg Nothing subgroupTables

type MCosetTable s = MVector s [Perm]

buildTables :: Int -> [Perm] -> Vector [Perm]
buildTables deg generators = create $ do
  table <- MV.replicate (deg-1) [identity deg]
  let filters = V.generate (deg-1) (flip fixes)
  let go_build (alpha:queue) = do
	result <- filter deg table filters alpha
	go_build (queue ++ result)
      go_build [] = return ()
  go_build generators
  return table

filter :: Int -> MCosetTable s -> Vector (Perm -> Bool) -> Perm -> ST s [Perm]
filter !deg !table !member = go_filter 0 where
  go_filter !i !alpha
    | i >= MV.length table = return []
    | otherwise = do
	gammas <- read table i
	case [alpha' | gamma <- gammas, let alpha' = inverse gamma * alpha, V.unsafeIndex member i alpha'] of
	  [] -> do
	    write table i (alpha:gammas)
	    rest <- fmap L.concat $ M.sequence [read table j | j <- [i..MV.length table - 1]]
	    prev <- fmap L.concat $ M.sequence [read table j | j <- [0..i-1]]
	    return ([gamma * alpha | gamma <- rest] ++ [alpha * gamma | gamma <- prev])
	  (alpha':_) -> go_filter (i+1) alpha'

fixes :: Perm -> Int -> Bool
fixes p i = p ! i == i

exhaustive :: Int -> [Perm] -> [Perm]
exhaustive deg gens = update [identity deg]
  where update xs = let
	    ys = L.nub (xs ++ [y | x <- xs, g <- gens, y <- [g * x, x * g, inverse g * x, x * inverse g]])
	    in if L.length ys > L.length xs then update ys else xs

constructOrbits :: Int -> [Perm] -> Vector IS.IntSet
constructOrbits !deg gens = L.foldl' propagate orbits0 [0..deg-1]
  where orbits0 = V.generate deg (\ i -> foldr IS.insert (IS.singleton i) [p ! i | p <- gens])
	propagate orbits j = V.accum IS.union orbits 
	    [(i, V.unsafeIndex orbits k) | let reaches = V.unsafeIndex orbits j,
		i <- IS.toList reaches, k <- IS.toList reaches, i /= k]