module Scrabble (scoreLetter, scoreWord) where

import Data.Char  (toUpper)
import Data.Maybe (listToMaybe)


scoreLetter :: Char -> Integer
scoreLetter = maybe 0 snd . scoreOf . toUpper

scoreOf :: Char -> Maybe (String, Integer)
scoreOf l = listToMaybe $ filter (elem l . fst) scrabbleScores

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter

scrabbleScores :: [(String, Integer)]
scrabbleScores = [("AEIOULNRST", 1)
                 , ("DG", 2)
                 , ("BCMP", 3)
                 , ("FHVWY", 4)
                 , ("K", 5)
                 , ("JX", 8)
                 , ("QZ", 10)]
