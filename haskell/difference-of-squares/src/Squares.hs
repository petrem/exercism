module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum n = let _sum = quot (n * (n + 1)) 2 in _sum * _sum

sumOfSquares :: Integral a => a -> a
sumOfSquares n = quot (n * (n + 1) * (2 * n + 1)) 6
