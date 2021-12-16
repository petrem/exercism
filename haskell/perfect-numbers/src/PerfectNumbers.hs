module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)


properDivisors :: Int -> [Int]
properDivisors x = [y | y <- [1..x-2], x `mod` y == 0]

aliquotSum :: Int -> Int
aliquotSum = sum . properDivisors

classify :: Int -> Maybe Classification
classify x
  | x <= 0 = Nothing
  | otherwise = Just $ case compare <$> aliquotSum <*> id $ x of
                  LT -> Deficient
                  EQ -> Perfect
                  GT -> Abundant
