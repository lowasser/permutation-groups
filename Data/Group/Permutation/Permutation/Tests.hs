module Data.Group.Permutation.Permutation.Tests (tests) where

import Control.Monad
import Test.QuickCheck

import Data.Group.Permutation.Permutation
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as P

import Prelude hiding ((*))

genPerm :: Int -> Gen Perm
genPerm n = do
  swaps <- vectorOf n $ choose (0, n-1)
  let perm = P.modify (\ xs -> sequence_ [P.swap xs i j | (i, j) <- zip [0..] swaps]) (P.enumFromN 0 n)
  return $ mkPerm n ((P.!) perm)

compositionProp :: Property
compositionProp = printTestCase "Composition" $ do
  Positive n <- arbitrary
  p1 <- genPerm n
  p2 <- genPerm n
  return $ printTestCase (show (p1, p2)) $ mkPerm n ((p1 !) . (p2 !)) == p1 * p2

tests :: [Property]
tests = [compositionProp]
