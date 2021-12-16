module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
  | not $ isDivisibleBy 4 = False
  | isDivisibleBy 100 = isDivisibleBy 400
  | otherwise = True
    where isDivisibleBy = (== 0) . (year `rem`)

