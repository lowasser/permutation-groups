module Data.Group.Permutation.Permutation where

import Data.Vector.Primitive

import Prelude hiding (length, (@), map)

newtype Perm = Perm {getPerm :: Vector Int} -- p[i] is where i goes

inverse :: Perm -> Perm
inverse (Perm p) = Perm (backpermute (enumFromN 0 n) p)
  where n = length p

degree :: Perm -> Int
degree (Perm p) = length p

(@) :: Perm -> Perm -> Perm
Perm p @ Perm q = Perm (map (unsafeIndex q) p)