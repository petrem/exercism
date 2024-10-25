module BinarySearch (find) where

import Data.Array

find :: (Ord a) => Array Int a -> a -> Maybe Int
find ary x = uncurry go $ bounds ary
  where
    go left right
      | right < left = Nothing
      | otherwise =
          let middle = (left + right) `div` 2
           in case compare x (ary ! middle) of
                EQ -> Just middle
                LT -> go left (middle - 1)
                GT -> go (middle + 1) right
