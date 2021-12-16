module Bob (responseFor) where

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Char as C
import qualified Data.Text as T
import           Data.Text (Text)


responseFor :: Text -> Text
responseFor xs
  | isQuestion .&&. isShout $ xs = "Calm down, I know what I'm doing!"
  | isQuestion xs = "Sure."
  | isShout xs = "Whoa, chill out!"
  | isSilence xs = "Fine. Be that way!"
  | otherwise = "Whatever."

infixr 3 .&&.
(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&&. p2 = (&&) <$> p1 <*> p2

isQuestion :: Text -> Bool
isQuestion = T.isSuffixOf "?" . T.stripEnd

isShout :: Text -> Bool
isShout = T.any C.isUpper .&&. not . T.any (C.isLetter .&&. C.isLower)

isSilence :: Text -> Bool
isSilence = T.all C.isSpace
