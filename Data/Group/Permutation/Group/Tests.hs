module Data.Group.Permutation.Group.Tests where

import Test.QuickCheck
import Data.Group.Permutation.Group
import Data.Group.Permutation.Permutation
import Data.Group.Permutation.Permutation.Tests (genPerm)
import qualified Data.Vector as V

import Data.List (foldl')

import Prelude hiding ((^), (*))

mkGroupElement :: Int -> V.Vector Perm -> Gen Perm
mkGroupElement deg generators = sized $ \ m -> do
  k <- choose (0, m)
  factors <- vectorOf k $ do
    i <- choose (0, V.length generators - 1)
    j <- arbitrary
    return (V.unsafeIndex generators i ^ j)
  return $ foldl' (*) (identity deg) factors

groupElemProp :: Int -> Property
groupElemProp deg = printTestCase "Group element" $ sized $ \ m -> do
  nGens <- choose (1, m)
  gens <- fmap (V.fromListN nGens) $ vectorOf nGens (genPerm deg)
  let g = permutationGroup deg (V.toList gens)
  x <- mkGroupElement deg gens
  return $ printTestCase (show (x, g)) $ member x g

tests :: Property
tests = property $ sized $ \ m0 -> resize (floor (sqrt (realToFrac m0))) $ sized $ \ m -> do
  deg <- choose (1, m)
  return $ (deg > 0) ==> (conjoin $ map ($ deg) [groupElemProp])
