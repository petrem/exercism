module CollatzConjectureRecursive (collatz) where

-- Initial, recursive, version
collatz :: Integer -> Maybe Integer
collatz x
  | x <= 0    = Nothing
  | otherwise = (Just . steps) x
  where steps y
          | y == 1    = 0
          | even y    = 1 + steps (y `div` 2)
          | otherwise = 2 + steps ((y * 3 + 1) `div` 2)

