module Anagram (anagramsFor) where

import qualified Data.Char as Char

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap


type Counter =  IntMap Int


incrementCounter :: IntMap.Key -> Counter -> Counter
incrementCounter = IntMap.alter setCounter
  where
    setCounter (Just x) = Just (x+1)
    setCounter Nothing = Just 1

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter isAnagram
  where
    isAnagram = (&&) <$> (toLower word /=) . toLower <*> (anagramSignature word ==) . anagramSignature
    toLower = map Char.toLower
    anagramSignature = foldr (incrementCounter . Char.ord . Char.toLower) IntMap.empty
