{-# LANGUAGE BangPatterns #-}
module Data.Group.Permutation.Group where

import Control.Exception.Base

import qualified Control.Monad as M
import qualified Data.List as L

import Data.Group.Permutation.Permutation
import Data.Vector.Mutable
import qualified Data.Vector.Mutable as MV

import Prelude hiding (read)

type MCosetTable s = MVector s [Perm]

buildTables :: Int -> [Perm] -> Vector [Perm]
buildTables !deg generators = assert (L.all (\ g -> degree g == deg)) $ do
  table <- MV.new (deg-1)
  let go_build (alpha:queue) = do
	result <- filter alpha 0 table
	go_build (queue' ++ result)
      go_build [] = return ()
  go_build generators
  

filter :: Perm -> Int -> MCosetTable -> ST s [Perm]
filter !alpha !i !table
  | i >= MV.length table = return []
  | otherwise = do
      gammas <- read table i
      case [alpha' | gamma <- gammas, let alpha' = inverse gamma * alpha, fixes alpha' (i+1)] of
	[] -> do
	  write table i (alpha:gammas)
	  rest <- liftM L.concat $ M.sequence [read table j | j <- [i+1..MV.length table - 1]]
	  return [alpha * gamma | gamma <- gammas ++ rest]
	(alpha':_) -> filter alpha' (i+1) table

fixes :: Perm -> Int -> Bool
fixes p i = p ! i == i