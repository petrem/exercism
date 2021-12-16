module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n = [row i | i <- [0..(n-1)]]

row :: Int -> [Integer]
row 0 = [1]
row n = 1:middle ++ [1] where
  middle = zipWith (+) <$> take (n - 1) <*> drop 1 $ row (n - 1)
