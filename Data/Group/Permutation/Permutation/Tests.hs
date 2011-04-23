module Data.Group.Permutation.Permutation.Tests (tests) where

import Control.Monad
import Test.QuickCheck

import Data.Group.Permutation.Permutation
import Data.Group.Permutation.Tests.Utils
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as P

import Prelude hiding ((*))

compositionProp :: Int -> Property
compositionProp n = printTestCase "Composition" $ do
  p1 <- genPerm n
  p2 <- genPerm n
  return $ printTestCase (show (p1, p2)) $ mkPerm n ((p1 !) . (p2 !)) == p1 * p2

inverseProp :: Int -> Property
inverseProp n = printTestCase "Inverse" $ do
  p <- genPerm n
  let pX = inverse p; i = identity n
  return $ printTestCase (show p) $ conjoin [i == p * pX, i == pX * p]

identityProp :: Int -> Property
identityProp n = printTestCase "Identity" $ do
  p <- genPerm n
  let i = identity n
  return $ printTestCase (show p) $ conjoin [p * i == p, i * p == p]

associativeProp :: Int -> Property
associativeProp n = printTestCase "Associative" $ do
  a <- genPerm n
  b <- genPerm n
  c <- genPerm n
  return $ printTestCase (show (a, b, c)) $ (a * b) * c == a * (b * c)

injective :: Int -> Property
injective n = printTestCase "Injectivity" $ do
  p <- genPerm n
  i <- choose (0, n-1)
  j <- choose (0, n-1)
  return $ printTestCase (show (p, i, j)) $ (i == j) == ((p ! i) == (p ! j))

tests :: Property
tests = property $ sized $ \ m -> do
  n <- choose (1, m)
  return $ (n > 0) ==> (conjoin $ map ($ n) [compositionProp, inverseProp, identityProp, associativeProp, injective])
