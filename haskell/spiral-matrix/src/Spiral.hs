module Spiral (spiral) where

import Data.List (foldl')

spiral :: Int -> [[Int]]
spiral 0 = []
spiral 1 = [[1]]
spiral n =
  padMap [1 .. n] [3 * (n - 1) + 1, 3 * (n - 1) .. 2 * (n - 1) + 1] id $
    let (_, _, res) = foldl' (\(l, r, acc) xs -> (l - 1, r + 1, padMap l r (perimeter +) xs : acc)) (perimeter, n + 1, []) $ spiral (n - 2)
     in reverse res
  where
    perimeter = 4 * (n - 1)

padMap :: a -> a -> (a -> a) -> [a] -> [a]
padMap x0 xN f = (x0 :) . foldr ((:) . f) [xN]
