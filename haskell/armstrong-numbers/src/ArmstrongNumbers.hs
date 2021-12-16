module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)

armstrong :: (Show a, Integral a) => a -> Bool
armstrong x = fromIntegral x == sum (map ((^ nDigits) . digitToInt) digits)
  where digits = show x
        nDigits = length digits
