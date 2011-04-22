{-# LANGUAGE BangPatterns #-}
module Data.Group.Permutation.Permutation (Perm, inverse, degree, (*), (!), mkPerm, identity) where

import Control.Exception.Base
import Data.Vector.Primitive hiding ((!))
import qualified Data.Vector.Primitive as P

import Prelude hiding (length, (*), map, all)

infixr 6 *

newtype Perm = Perm {getPerm :: Vector Int} deriving (Eq, Show) -- p[i] is where i goes

inverse :: Perm -> Perm
inverse (Perm p) = Perm (unsafeBackpermute (enumFromN 0 n) p)
  where n = length p

degree :: Perm -> Int
degree (Perm p) = length p

(*) :: Perm -> Perm -> Perm
Perm !p * Perm q = assert (length p == length q) $
  Perm (map (P.unsafeIndex p) q)

mkPerm :: Int -> (Int -> Int) -> Perm
mkPerm k p = let arr = generate k p in
  assert (all (< k) arr) $ Perm arr

(!) :: Perm -> Int -> Int
(!) (Perm p) i = assert (i < length p) $ P.unsafeIndex p i

identity :: Int -> Perm
identity n = assert (n > 0) $ Perm (enumFromN 0 n)
