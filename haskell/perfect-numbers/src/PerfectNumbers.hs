module PerfectNumbers (classify, Classification (..)) where

data Classification = Deficient | Perfect | Abundant deriving (Enum, Eq, Show)

classify :: Int -> Maybe Classification
classify x = toEnum . fromEnum . compare (sum properDivisors) <$> if x <= 0 then Nothing else Just x
  where
    properDivisors = [y | y <- [1 .. x - 2], x `mod` y == 0]
