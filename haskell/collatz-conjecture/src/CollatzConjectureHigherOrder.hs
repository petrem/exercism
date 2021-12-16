module CollatzConjectureHigherOrder (collatz) where

-- Second version, based on higher-order functions
hailstone :: Integer -> Integer
hailstone x
  | even x    = x `div` 2
  | otherwise = x * 3 + 1

collatz :: Integer -> Maybe Integer
collatz x
  | x <= 0    = Nothing
  | otherwise = Just . toInteger . length . takeWhile (/=1) . iterate hailstone $ x
