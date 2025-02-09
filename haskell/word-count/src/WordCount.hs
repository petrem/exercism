{-# LANGUAGE OverloadedStrings #-}

module WordCount (wordCount) where

import Data.Char (isAlphaNum)
import Data.MultiSet (MultiSet, Occur)
import qualified Data.MultiSet as M
import Data.Text (Text)
import qualified Data.Text as T

wordCount :: Text -> [(Text, Occur)]
wordCount = M.toOccurList . foldr M.insert M.empty . words' . T.toLower

isSeparator :: Char -> Bool
isSeparator = not . ((||) <$> Data.Char.isAlphaNum <*> (== '\''))

words' :: Text -> [Text]
words' s = case T.dropWhile isSeparator s of
  "" -> []
  s' -> T.dropAround (== '\'') w : words' s''
    where
      (w, s'') = T.break isSeparator s'
