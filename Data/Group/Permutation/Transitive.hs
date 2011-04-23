{-# LANGUAGE BangPatterns #-}
module Data.Group.Permutation.Transitive where

import qualified Data.Vector as V
import qualified Data.IntSet as IS
import Data.Group.Permutation.Group

import Prelude hiding (read)

isTransitive :: PermGroup -> Bool
isTransitive g = IS.size (V.unsafeIndex (orbits g) 0) == deg g
