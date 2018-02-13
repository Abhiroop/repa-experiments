{-# LANGUAGE BangPatterns #-}
module Experiments where

import Data.Array.Repa as Repa
import Data.Functor.Identity

type Weight = Int
type Graph r = Array r DIM2 Weight

shortestPath :: Graph U -> Graph U
shortestPath g0 = runIdentity $ go g0 0
  where Z :. _ :. n = extent g0 -- this is basically giving dimensions
        go !g !k
          | k == n = return g
          | otherwise = do
              g' <- computeP (fromFunction (Z:.n:.n) sp)
              go g' (k + 1)
          where
            sp (Z:.i:.j) = min (g ! (Z:.i:.j)) (g ! (Z:.i:.k) + g ! (Z:.k:.j))

maxDistance :: Weight -> Weight -> Weight
maxDistance x y
  | x == maxBound = y
  | y == maxBound = x
  | otherwise     = max x y

maxDistances :: Graph U -> Array U DIM1 Weight
maxDistances = foldS maxDistance maxBound
