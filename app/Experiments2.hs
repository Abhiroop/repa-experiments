module Experiments2 where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native as CPU

type Weight = Int32
type Graph  = Array DIM2 Weight


step :: Acc (Scalar Int) -> Acc Graph -> Acc Graph
step k g = generate (shape g) sp
  where k' = the k

        sp :: Exp DIM2 -> Exp Weight
        sp ix = let (Z :. i :. j) = unlift ix
                  in A.min (g ! (index2 i j)) (g ! (index2 i k') + g ! (index2 k' j))

shortestPathAcc :: Int -> Acc Graph -> Acc Graph
shortestPathAcc n g0 = foldl1 (>->) steps g0
  where steps :: [Acc Graph -> Acc Graph]
        steps = [step (unit (constant k)) | k <- [0 .. n - 1]]

shortestPath :: Graph -> Graph
shortestPath g0 = run (shortestPathAcc n (use g0))
  where
    Z :. _ :. n = arrayShape g0
