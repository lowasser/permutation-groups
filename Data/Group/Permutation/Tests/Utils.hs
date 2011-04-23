module Data.Group.Permutation.Tests.Utils where

import Test.QuickCheck

import Data.Group.Permutation.Permutation
import Data.Group.Permutation.Group
import Data.List (foldl')
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as P
import qualified Data.Vector as V

import Prelude hiding ((*), (^))

mkGroupElement :: Int -> V.Vector Perm -> Gen Perm
mkGroupElement deg generators = sized $ \ m -> do
  k <- choose (0, m)
  factors <- vectorOf k $ do
    i <- choose (0, V.length generators - 1)
    j <- arbitrary
    return (V.unsafeIndex generators i ^ j)
  return $ foldl' (*) (identity deg) factors

genPerm :: Int -> Gen Perm
genPerm n = do
  swaps <- vectorOf n $ choose (0, n-1)
  let perm = P.modify (\ xs -> sequence_ [P.swap xs i j | (i, j) <- zip [0..] swaps]) (P.enumFromN 0 n)
  return $ mkPerm n ((P.!) perm)

genGroup :: Int -> Gen (PermGroup, Gen Perm)
genGroup deg = sized $ \ m -> do
  nGens <- choose (1, m)
  gens <- fmap (V.fromListN nGens) $ vectorOf nGens (genPerm deg)
  let g = permutationGroup deg (V.toList gens)
  return (g, mkGroupElement deg gens)