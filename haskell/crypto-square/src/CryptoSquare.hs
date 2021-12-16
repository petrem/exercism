module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)
import Data.List.Split (chunksOf)


encode :: String -> String
encode xs = unwords (transpose . chunksOf cols $ padded)
  where
    normalized = map toLower $ filter isAlphaNum xs
    cols = ceiling (sqrt (len normalized))
    rows = ceiling (len normalized / fromIntegral cols)
    padded = take (cols * rows) $ normalized ++ repeat ' '

len :: Num b => [a] -> b
len = fromIntegral . length
