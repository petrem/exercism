module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.

import Data.List (unfoldr)
import Prelude hiding (div, divMod, mod, quot, quotRem, rem, (/))

primesUpTo :: Integer -> [Integer]
primesUpTo n
  | n < 2 = []
  | otherwise = unfoldr sieveGen (0, [])
  where
    multiplesOf x = [x + x, x + x + x .. n]

    sieveGen (0, _) = Just (2, (2, multiplesOf 2))
    sieveGen (prev, marked) = ((,) <$> fst <*> id) <$> genNext (prev + 1)
      where
        genNext candidate
          | candidate > n = Nothing
          | isPrime candidate = Just (candidate, multiplesOf candidate ++ marked)
          | otherwise = genNext (candidate + 1)

        isPrime candidate = candidate `notElem` marked

-- '(,) <$> fst <*> id'  can be written '(,) =<< fst'
