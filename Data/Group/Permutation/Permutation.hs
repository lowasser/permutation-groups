{-# LANGUAGE BangPatterns #-}
module Data.Group.Permutation.Permutation (Perm, inverse, degree, (*), (!), (^), mkPerm, identity, cycPerm) where

import qualified Control.Monad as M
import Control.Monad.ST

import qualified Data.List as L
import Data.Bits

import Control.Exception.Base
import Data.Vector.Primitive hiding ((!), (++))
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as PM
import qualified Data.Vector.Unboxed.Mutable as U

import Prelude hiding (length, (*), (^), map, all)

infixr 6 *

newtype Perm = Perm {getPerm :: Vector Int} deriving (Eq) -- p[i] is where i goes

instance Show Perm where
  showsPrec _ perm rest = runST (cycleNotation perm rest)

inverse :: Perm -> Perm
inverse (Perm p) = Perm (unsafeUpdate_ (P.replicate n 0) p (P.enumFromN 0 n))
  where n = length p

degree :: Perm -> Int
degree (Perm p) = length p

(*) :: Perm -> Perm -> Perm
Perm !p * Perm q = assert (length p == length q) $
  Perm (map (P.unsafeIndex p) q)

(^) :: Perm -> Int -> Perm
p ^ n = case compare n 0 of
    LT -> do_pow (inverse p) (-n)
    EQ -> identity (degree p)
    GT -> do_pow p n
  where do_pow !p 0 = identity (degree p)
	do_pow !p 1 = p
	do_pow !p k
	  | even k = do_pow (p * p) (k `shiftR` 1)
	  | otherwise = p * do_pow (p * p) (k `shiftR` 1)

mkPerm :: Int -> (Int -> Int) -> Perm
mkPerm k p = let arr = generate k p in
  assert (all (< k) arr) $ Perm arr

(!) :: Perm -> Int -> Int
(!) (Perm p) i = assert (i < length p) $ P.unsafeIndex p i

identity :: Int -> Perm
identity n = assert (n > 0) $ Perm (enumFromN 0 n)

cycPerm :: Int -> [[Int]] -> Perm
cycPerm n cycles = Perm $ modify (\ mv -> M.forM_ cycles $ \ (c:cyc) -> M.zipWithM_ (PM.write mv) (c:cyc) (cyc ++ [c])) (P.enumFromN 0 n)

cycleNotation :: Perm -> String -> ST s String
cycleNotation p rest = do
  done <- U.replicate n False
  let find_cycle i cont
	| i >= n  = cont (":" ++ shows n ("]" ++ rest))
	| otherwise  = do
	    doneI <- U.read done i
	    if doneI then find_cycle (i+1) cont else do
	      cyc <- get_cycle i i
	      case cyc of
		[_] -> find_cycle (i+1) cont
		_ -> find_cycle (i+1) (\ str -> cont $ "(" ++ L.intercalate " " (L.map show cyc) ++ ")" ++ str)
      get_cycle i start = do
	let i' = p ! i
	U.write done i True
	if i' == start then return [i] else do
	  fmap (i:) (get_cycle i' start)
  find_cycle 0 (\ str -> return ("[" ++ str))
  where n = degree p

{-# RULES
--      "inverse/*" forall p q . inverse p * q = let n = degree p in Perm (unsafeUpdate_ (P.replicate n 0) (getPerm p) (getPerm q))
  #-}
