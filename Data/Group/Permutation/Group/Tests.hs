module Data.Group.Permutation.Group.Tests (tests) where

import Test.QuickCheck
import Data.Group.Permutation.Group
import Data.Group.Permutation.Permutation
import Data.Group.Permutation.Tests.Utils
import qualified Data.Vector as V

import Data.List (foldl')
import qualified Data.List as L
import qualified Data.IntSet as IS

import Prelude hiding ((^), (*))

groupElemProp :: Int -> Property
groupElemProp deg = printTestCase "Group element" $ do
  (g, gElemGen) <- genGroup deg
  x <- gElemGen
  return $ printTestCase (show (x, g)) $ member x g

subGroupProp :: Int -> Property
subGroupProp deg = printTestCase "Subgroup" $ do
  (g, _) <- genGroup deg
  x <- genPerm deg
  let h = permutationGroup deg (x:V.toList (generators g))
  return $ printTestCase (show (x, g, x `member` g)) $ (x `member` g) == (h `isSubgroup` g)

orderProp :: Int -> Property
orderProp deg = printTestCase "Order" $ sized $ \ m -> resize (floor (sqrt (fromIntegral m))) $ do
  (g, _) <- genGroup deg
  let gens = V.toList (generators g)
  return $ printTestCase (show (g, cosetTables g, order g)) $ length (exhaustive deg gens) == order g

orbitProp :: Int -> Property
orbitProp deg = printTestCase "Orbits" $ sized $ \ m -> do
  nGens <- choose (1, m)
  gens <- fmap (V.fromListN nGens) $ vectorOf nGens (genPerm deg)
  let g = permutationGroup deg (V.toList gens)
  x <- mkGroupElement deg gens
  return $ printTestCase (show (x, g)) $ L.all (\ i -> IS.member (x ! i) ((V.!) (orbits g) i)) [0..deg-1]

tests :: Property
tests = property $ sized $ \ m -> do
  deg <- choose (1, m)
  return $ (deg > 0) ==> (conjoin $ map ($ deg) [groupElemProp, orderProp, subGroupProp, orbitProp])
