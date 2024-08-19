module Roman
  ( numerals
  ) where

numerals :: Integer -> Maybe String
numerals number
  | number `between` (1, 4000) = Just $ getRomanDigits number
  | otherwise = Nothing
  where
    between x (l, r) = x >= l && x < r -- note: inclusive on the left
    getRomanDigits n
      | n `between` (900, 1000) || n `between` (400, 500) =
        'C' : getRomanDigits (n + 100)
      | n `between` (90, 100) || n `between` (40, 50) = 'X' : getRomanDigits (n + 10)
      | n == 9 || n == 4 = 'I' : getRomanDigits (n + 1)
      | n >= 1000 = 'M' : getRomanDigits (n - 1000)
      | n >= 500 = 'D' : getRomanDigits (n - 500)
      | n >= 100 = 'C' : getRomanDigits (n - 100)
      | n >= 50 = 'L' : getRomanDigits (n - 50)
      | n >= 10 = 'X' : getRomanDigits (n - 10)
      | n >= 5 = 'V' : getRomanDigits (n - 5)
      | n >= 1 = 'I' : getRomanDigits (n - 1)
      | otherwise = ""
{-
-- Previous version
import Data.Char (digitToInt)


numerals :: Integer -> Maybe String
numerals n
  | n <= 0 || n > 3999 = Nothing
  | otherwise          = Just getRomanDigits
  where
    arabicDigits = show n
    getRomanDigit d rds = rds !! digitToInt d
    getRomanDigits = concat $ reverse $ zipWith getRomanDigit (reverse arabicDigits) romanDigits
    romanDigits :: [[String]]
    romanDigits = [["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
                  ,["", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"]
                  ,["", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"]
                  ,take 4 $ iterate (++ "M") ""
                  ]
-}
