{-# LANGUAGE OverloadedStrings #-}

module WordCount (wordCount) where

import Data.Char (isAlphaNum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

wordCount :: Text -> Map Text Int
wordCount = countWords . words' . T.toLower

countWords :: [Text] -> Map Text Int
countWords = foldr (\s r -> M.insertWith (+) s 1 r) M.empty

isSeparator :: Char -> Bool
isSeparator = not . ((||) <$> Data.Char.isAlphaNum <*> (== '\''))

words' :: Text -> [Text]
words' s = case T.dropWhile isSeparator s of
  "" -> []
  s' -> peelApostrophes w : words' s''
    where
      (w, s'') = T.break isSeparator s'

peelApostrophes :: Text -> Text
peelApostrophes = T.dropAround (== '\'')

-- TODO:
-- - look into Multiset
