{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module GameOfLife (tick) where

import Data.Array
import Data.Ix

tick :: [[Int]] -> [[Int]]
tick [] = []
tick xss = fromBorderedArray . step . toBorderedArray 0 $ xss

-- |  Build an array from list of lists, padded with a "zero"
toBorderedArray :: a -> [[a]] -> Array (Int, Int) a
toBorderedArray z a = array ((0, 0), (n', m')) (zeros ++ values)
  where
    zeros = map (,z) (concat zeroIndexes)
    zeroIndexes =
      [ range ((0, 0), (0, m')),
        range ((n', 0), (n', m')),
        range ((0, 0), (n', 0)),
        range ((0, m'), (n', m'))
      ]
    values = zip (range ((1, 1), (n, m))) (concat a)
    (n, m) = getDimensions a
    (n', m') = (n + 1, m + 1)
    getDimensions a = (length a, length . head $ a)

fromBorderedArray :: Array (Int, Int) a -> [[a]]
fromBorderedArray a = [[a ! (i, j) | j <- [1 .. (m - 1)]] | i <- [1 .. (n - 1)]]
  where
    (_, (m, n)) = bounds a

step :: (Eq a, Num a) => Array (Int, Int) a -> Array (Int, Int) a
step a = b
  where
    b = array (bounds a) [((i, j), cellFate i j) | (i, j) <- range ((1, 1), (n - 1, m - 1))]

    cellFate row col = decide (a ! (row, col)) (aliveNeighbors row col)

    decide Alive 2 = Alive
    decide Alive 3 = Alive
    decide Dead 3 = Alive
    decide _ _ = Dead

    aliveNeighbors row col =
      sum $
        [a ! (row + k, col + l) | k <- [-1 .. 1], l <- [-1 .. 1], k /= 0 || l /= 0]

    (_, (n, m)) = bounds a

pattern Alive = 1

pattern Dead = 0
