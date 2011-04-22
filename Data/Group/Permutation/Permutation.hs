{-# LANGUAGE BangPatterns #-}
module Data.Group.Permutation.Permutation (Perm, inverse, degree, (*), (!), mkPerm, identity) where

import Control.Monad.ST

import qualified Data.List as L

import Control.Exception.Base
import Data.Vector.Primitive hiding ((!))
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed.Mutable as U

import Prelude hiding (length, (*), map, all)

infixr 6 *

newtype Perm = Perm {getPerm :: Vector Int} deriving (Eq) -- p[i] is where i goes

instance Show Perm where
  showsPrec _ perm rest = runST (cycleNotation perm rest)

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

cycleNotation :: Perm -> String -> ST s String
cycleNotation p rest = do
  done <- U.replicate n False
  let find_cycle i cont
	| i >= n  = cont ("]" ++ rest)
	| otherwise  = do
	    doneI <- read done i
	    if doneI then find_cycle (i+1) cont else do
	      cyc <- get_cycle i i
	      case cyc of
		[_] -> find_cycle (i+1) cont
		_ -> find_cycle (i+1) (\ str -> cont $ "(" ++ L.intercalate " " (L.map show cyc) ++ ")" ++ str)
      get_cycle i start = do
	let i' = p ! i
	if i' == start then return [] else do
	  write done i True
	  liftM (i:) (get_cycle i' start)
  find_cycle 0 (\ str -> return ("[" ++ str))
  where n = degree p