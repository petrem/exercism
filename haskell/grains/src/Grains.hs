module Grains
  ( square
  , total
  ) where

import Data.Maybe (fromJust, mapMaybe)

default (Integer)

square :: Integer -> Maybe Integer
square n
  | 1 <= n && n <=64  = Just (2^(n - 1))
  | otherwise         = Nothing

total :: Integer
total = sum [2^x | x <-[0..63]]

  -- which is 2^63 + 2^62 .. +2^1 + 2^0
  -- which is, in binary (left) and hex (right)
  -- 1000....0000 +    0x8000000000000000 +
  -- 0100....0000 +    0x0400000000000000 +
  -- 0010....0000 +    0x0200000000000000 +
  -- 0001....0000 +    0x0100000000000000 +
  -- ... and so on
  -- 0000....0010 +    0x0000000000000002 +
  -- 0000....0001 =    0x0000000000000001 =
  -- ------------      --------------------
  -- 1111....1111      0xffffffffffffffff
  --
  -- So total = 0xffffffffffffffff == 18446744073709551615

-- or a version that re-uses squere, to cover my bases:

total' :: Integer
total' = fromJust (sum <$> traverse square [1..64])

total'' :: Integer
total'' = sum . mapMaybe square $ [1..64]
