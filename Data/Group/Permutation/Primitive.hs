{-# LANGUAGE BangPatterns #-}
module Data.Group.Permutation.Primitive (
  Block, BlockSystem,
  isTrivial, stabilizesBlock, blocksCorrectly,
  minimalBlockSystem) where

import Control.Monad.ST
import Control.Monad

import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as PM
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Fusion.Stream as Stream
import qualified Data.Vector as V

import Data.Group.Permutation.Permutation
import Data.Group.Permutation.Group

type Block = Int
type BlockSystem = P.Vector Block

replaceWith :: P.MVector s Int -> Int -> Int -> ST s ()
replaceWith !mv !i !j = void $ GM.transform (fmap (\ x -> if x == i then j else x)) mv

stabilizesBlock :: Perm -> BlockSystem -> Block -> Bool
stabilizesBlock p system block =
  P.ifoldr (\ i b x -> (b /= block || P.unsafeIndex system (p ! i) == b) && x) True system

isTrivial :: BlockSystem -> Bool
isTrivial system = P.all (== 0) system || P.ifoldr (\ i b x -> i == b && x) True system

blocksCorrectly :: Perm -> BlockSystem -> Bool
blocksCorrectly p@(Perm perm) system = let
  nBlocks = P.maximum system + 1
  !mappedTo = P.update_ (P.replicate nBlocks (-1)) system 
    (P.backpermute system perm)
  in P.ifoldr (\ i b x -> P.unsafeIndex mappedTo b == P.unsafeIndex system (p ! i) && x) True system

minimalBlockSystem :: PermGroup -> BlockSystem
minimalBlockSystem g = refiner g (P.enumFromN 0 (deg g)) 0

refiner :: PermGroup -> BlockSystem -> Int -> BlockSystem
refiner !g !system !i
  | i >= deg g	= system
  | P.unsafeIndex system i == 0
      = refiner g system (i+1)
  | otherwise
      = let system' = imprimitiveBlock g system i 0
	  in refiner g (if isTrivial system' then system else system') (i+1)

imprimitiveBlock :: PermGroup -> BlockSystem -> Int -> Block -> BlockSystem
imprimitiveBlock !g !f0 !omega !block = P.modify (\ f -> do
  let d = deg g
  PM.write f omega block
  let go (beta:c0) = do
	alpha <- PM.read f beta
	let proc c !gi = do
	      fgamma0 <- PM.read f (gi ! alpha)
	      fdelta0 <- PM.read f (gi ! beta)
	      if fgamma0 == fdelta0 then return c else case orderPair fgamma0 fdelta0 of
		(fdelta, fgamma) -> do
		  replaceWith f fgamma fdelta
		  return (fgamma:c)
	c' <- V.foldM proc c0 (generators g)
	go c'
      go [] = return ()
  go [omega]) f0

orderPair :: Ord a => a -> a -> (a, a)
orderPair a b
  | a <= b	= (a, b)
  | otherwise	= (b, a)