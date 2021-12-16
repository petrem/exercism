module Roman (numerals) where

import Data.Char (digitToInt)

numerals :: Integer -> Maybe String
numerals n
  | n <= 0 || n > 3999 = Nothing
  | otherwise          = Just $ getRomanDigits
  where
    arabicDigits = show n
    getRomanDigit d rds = rds !! digitToInt d
    getRomanDigits = concat $ reverse $ zipWith getRomanDigit (reverse arabicDigits) romanDigits


romanDigits :: [[String]]
romanDigits = [["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
              ,["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"]
              ,["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"]
              ,take 4 $ iterate (\xs -> xs ++ "M") ""
              ]
