{-# LANGUAGE BangPatterns, RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Group.Permutation.Group (PermGroup, permutationGroup, member, exhaustive) where

import Control.Exception.Base
import Control.Monad.ST

import qualified Control.Monad as M
import qualified Data.List as L

import Data.Group.Permutation.Permutation
import Data.Vector (Vector, create)
import qualified Data.Vector as V
import Data.Vector.Mutable
import qualified Data.Vector.Mutable as MV

import Prelude hiding (read, filter, (*))

data PermGroup = Group {
  deg :: !Int,
  order :: Int,
  generators :: Vector Perm,
  cosetTables :: Vector (Vector Perm)}

instance Eq PermGroup where
  g1 == g2 
    | V.length (generators g1) <= V.length (generators g2)
      = deg g1 == deg g2 && order g1 == order g2 && V.all (`member` g2) (generators g1)
    | otherwise = g2 == g1

instance Show PermGroup where
  show Group{generators} = "<" ++ L.intercalate ", " (L.map show $ V.toList generators) ++ ">"

permutationGroup :: Int -> [Perm] -> PermGroup
permutationGroup !deg gens = assert (L.all (\ g -> degree g == deg) gens) $ let
  cosetTables = V.map V.fromList $ buildTables deg gens
  generators0 = V.fromList gens
  generators = generators0
  order = V.product (V.map V.length cosetTables)
  in Group{..}

member :: Perm -> PermGroup -> Bool
member alpha Group{cosetTables, deg} = assert (degree alpha == deg) $ member_loop alpha 0 where
  member_loop !alpha !i
    | i >= V.length cosetTables = True
    | otherwise = case V.find (`fixes` i) (V.map (\ gamma -> inverse gamma * alpha) (V.unsafeIndex cosetTables i)) of
	Just alpha' -> member_loop alpha' (i+1)
	Nothing -> False

type MCosetTable s = MVector s [Perm]

buildTables :: Int -> [Perm] -> Vector [Perm]
buildTables deg generators = create $ do
  table <- MV.replicate (deg-1) [identity deg]
  let go_build (alpha:queue) = do
	result <- filter deg alpha 0 table
	go_build (queue ++ result)
      go_build [] = return ()
  go_build (generators)
  return table

filter :: Int -> Perm -> Int -> MCosetTable s -> ST s [Perm]
filter !deg !alpha !i !table
  | i >= MV.length table = return []
  | otherwise = do
      gammas <- read table i
      case [alpha' | gamma <- gammas, let alpha' = inverse gamma * alpha, fixes alpha' i] of
	[] -> do
	  write table i (alpha:inverse alpha:gammas)
	  rest <- fmap L.concat $ M.sequence [read table j | j <- [i..MV.length table - 1]]
          prev <- fmap L.concat $ M.sequence [read table j | j <- [0..i-1]]
	  return ([gamma * alpha | gamma <- rest] ++ [alpha * gamma | gamma <- prev])
	(alpha':_) -> filter deg alpha' (i+1) table

fixes :: Perm -> Int -> Bool
fixes p i = p ! i == i

exhaustive :: Int -> [Perm] -> [Perm]
exhaustive deg gens = update [identity deg]
  where update xs = let
	    ys = L.nub (xs ++ [y | x <- xs, g <- gens, y <- [g * x, x * g, inverse g * x, x * inverse g]])
	    in if L.length ys > L.length xs then update ys else xs
	
