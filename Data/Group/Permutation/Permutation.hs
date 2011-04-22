module Data.Group.Permutation.Permutation where

import Data.Vector.Primitive
import qualified Data.Vector.Primitive as P

import Prelude hiding (length, (*), map)

newtype Perm = Perm {getPerm :: Vector Int} deriving (Eq) -- p[i] is where i goes

inverse :: Perm -> Perm
inverse (Perm p) = Perm (backpermute (enumFromN 0 n) p)
  where n = length p

degree :: Perm -> Int
degree (Perm p) = length p

(*) :: Perm -> Perm -> Perm
Perm p * Perm q = Perm (map ((P.!) q) p)

mkPerm :: Int -> (Int -> Int) -> Perm
mkPerm k p = Perm (generate k p)

(!) :: Perm -> Int -> Int
Perm p ! i = (P.!) p i

identity :: Int -> Perm
identity n = Perm (enumFromN 0 n)
