module Isogram (isIsogram, isIsogram') where

{-# LANGUAGE OverloadedStrings #-}


import qualified Data.Char as C
import qualified Data.Set  as S
import qualified Data.Text as T
import           Data.Text (Text)

isIsogram :: Text -> Bool
isIsogram = snd . (T.foldl helper (S.empty, True)) . T.toLower . (T.filter C.isAlphaNum)
  where helper acc x = (S.insert x (fst acc), (snd acc) && not (S.member x (fst acc)))


isIsogram' :: String -> Bool
isIsogram' =  areUnique . (filter C.isAlphaNum) . (map C.toLower)
  where areUnique = (==) <$> (length . id) <*> (length . S.fromList)
