module Beer (song) where

import Data.Char (toUpper)
import Data.List (intercalate)

song :: String
song = intercalate "\n" $ map stanza [99,98..0]

stanza :: Int -> String
stanza = (++) <$> verseOne <*> verseTwo
  where verseOne = versify <$> capitalize . bottlesOnTheWall <*> bottlesOfBeer
        verseTwo = versify <$> obtainBeer <*> bottlesOnTheWall . oneLess
        versify part1 part2 = (part1 ++ ", ") ++ (part2 ++ ".\n")
        bottlesOnTheWall = (++ " on the wall") . bottlesOfBeer
        bottlesOfBeer n = howMany ++ " bottle" ++ plural ++ " of beer"
          where howMany
                  | n >= 1    = show n
                  | otherwise = "no more"
                plural = if n /= 1 then "s" else ""
        obtainBeer n
          | n == 0    = "Go to the store and buy some more"
          | otherwise = let what = if n == 1 then "it" else "one" in
                          "Take " ++ what ++ " down and pass it around"
        capitalize = (:) <$> toUpper . head <*> tail
        oneLess n
          | n == 0    = 99
          | otherwise = n - 1
