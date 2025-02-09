module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

wordCount :: String -> Map String Int
wordCount = countWords . words' . fmap toLower

countWords :: [String] -> Map String Int
countWords = foldr (\s r -> M.insertWith (+) s 1 r) M.empty

isSeparator :: Char -> Bool
isSeparator = not . ((||) <$> Data.Char.isAlphaNum <*> (== '\''))

words' :: String -> [String]
words' s = case dropWhile isSeparator s of
  "" -> []
  s' -> peelApostrophes w : words' s''
    where
      (w, s'') = break isSeparator s'

peelApostrophes :: String -> String
peelApostrophes = dropWhile (== '\'') . foldr go ""
  where
    go '\'' [] = []
    go c acc = c : acc

-- TODO:
-- - use Data.Text
-- - look into Multiset
