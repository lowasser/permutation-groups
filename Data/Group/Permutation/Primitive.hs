{-# LANGUAGE BangPatterns #-}
module Data.Group.Permutation.Primitive where

import Control.Monad.ST
import Control.Monad

import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Primitive.Mutable as PM
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Fusion.Stream as Stream
import qualified Data.Vector as V

import Data.Group.Permutation.Permutation
import Data.Group.Permutation.Group

replaceWith :: P.MVector s Int -> Int -> Int -> ST s ()
replaceWith !mv !i !j = void $ GM.transform (fmap (\ x -> if x == i then j else x)) mv



imprimitiveBlock :: PermGroup -> P.Vector Int -> Int -> Int -> P.Vector Int
imprimitiveBlock !g !f0 !omega !block = P.create $ do
  let d = deg g
  f <- P.thaw f0
  PM.write f omega block
  let go (beta:c0) = do
	alpha <- PM.read f beta
	let proc c !gi = do
	      fgamma0 <- PM.read f (gi ! alpha)
	      fdelta0 <- PM.read f (gi ! beta)
	      if fgamma0 == fdelta0 then return c else case orderPair fgamma0 fdelta0 of
		(fdelta, fgamma) -> do
		  replaceWith f fgamma fdelta
		  return (fgamma:c)
	c' <- V.foldM proc c0 (generators g)
	go c'
      go [] = return ()
  go [omega]
  return f

orderPair :: Ord a => a -> a -> (a, a)
orderPair a b
  | a <= b	= (a, b)
  | otherwise	= (b, a)