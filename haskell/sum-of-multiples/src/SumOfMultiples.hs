module SumOfMultiples (sumOfMultiples) where

import qualified Data.Set as Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . Set.fromList $ factorsWithinLimit
  where
    factorsWithinLimit = [m * i | m <- factors, m /= 0, i <- [1 .. ((limit-1) `div` m)]]
