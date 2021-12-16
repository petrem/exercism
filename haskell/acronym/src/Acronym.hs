module Acronym (abbreviate) where

{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C


abbreviate :: Text -> Text
abbreviate = T.toUpper . T.filter C.isAlpha . snd . T.mapAccumL dispatch ' '
  where dispatch prev c
          | not $ C.isAlpha c = (c, ' ')
          | prev /= '\'' && (C.isSpace prev || C.isPunctuation prev) = (c, c)
          | C.isLower prev && C.isUpper c = (c, c)
          | otherwise = (c, ' ')
