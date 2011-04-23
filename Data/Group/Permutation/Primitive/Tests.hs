module Data.Group.Permutation.Primitive.Tests (tests) where

import Test.QuickCheck

import Data.Group.Permutation.Tests.Utils
import Data.Group.Permutation.Primitive
import Data.Group.Permutation.Transitive

blockSystemProp :: Int -> Property
blockSystemProp deg = property $ do
  (g, gElemGen) <- genGroup deg
  let system = minimalBlockSystem g
  return $ printTestCase (show (g, system)) $ (isTransitive g ==> (do
    x <- gElemGen
    return $ printTestCase (show x) $ blocksCorrectly x system))

tests :: Property
tests = property $ sized $ \ m -> do
  n <- choose (1, m)
  return $ (n > 0) ==> (conjoin $ map ($ n) [blockSystemProp])
