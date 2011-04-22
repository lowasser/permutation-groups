{-# LANGUAGE BangPatterns, RecordWildCards, NamedFieldPuns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Group.Permutation.Group (PermGroup, permutationGroup, member) where

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

permutationGroup :: Int -> [Perm] -> PermGroup
permutationGroup !deg gens = assert (L.all (\ g -> degree g == deg) gens) $ let
  cosetTables = V.map V.fromList $ buildTables deg gens
  generators = V.fromList gens
  order = V.product (V.map V.length cosetTables)
  in Group{..}

member :: Perm -> PermGroup -> Bool
member alpha Group{!cosetTables, deg} = assert (degree alpha == deg) $ member_loop alpha 0 where
  member_loop !alpha !i
    | i >= V.length cosetTables = return True
    | otherwise = case V.find (`fixes` i+1) (V.map (\ gamma -> inverse gamma * alpha) (V.unsafeIndex cosetTables i)) of
	Just alpha' -> member_loop alpha' (i+1)
	Nothing -> return False

type MCosetTable s = MVector s [Perm]

buildTables :: Int -> [Perm] -> Vector [Perm]
buildTables deg generators = create $ do
  table <- MV.new (deg-1)
  let go_build (alpha:queue) = do
	result <- filter alpha 0 table
	go_build (queue ++ result)
      go_build [] = return ()
  go_build generators
  return table

filter :: Perm -> Int -> MCosetTable s -> ST s [Perm]
filter !alpha !i !table
  | i >= MV.length table = return []
  | otherwise = do
      gammas <- read table i
      case [alpha' | gamma <- gammas, let alpha' = inverse gamma * alpha, fixes alpha' (i+1)] of
	[] -> do
	  write table i (alpha:gammas)
	  rest <- fmap L.concat $ M.sequence [read table j | j <- [i..MV.length table - 1]]
	  return [alpha * gamma | gamma <- rest]
	(alpha':_) -> filter alpha' (i+1) table

fixes :: Perm -> Int -> Bool
fixes p i = p ! i == i
