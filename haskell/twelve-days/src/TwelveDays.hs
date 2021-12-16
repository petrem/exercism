module TwelveDays (recite) where

import Data.List (intercalate)
import Text.Printf (printf)


newtype SInt = SInt { getInt::Int } deriving (Eq, Ord)

instance Show SInt where
  show i = case getInt i of
             1 -> "one"
             2 -> "two"
             3 -> "three"
             4 -> "four"
             5 -> "five"
             6 -> "six"
             7 -> "seven"
             8 -> "eight"
             9 -> "nine"
             10 -> "ten"
             11 -> "eleven"
             12 -> "twelve"
             _  -> error "I do not know how to represent " ++ show i

nth :: Int -> String
nth n = case n of
          1  -> "first"
          2  -> "second"
          3  -> "third"
          5  -> "fifth"
          8  -> "eighth"
          9  -> "ninth"
          12 -> "twelfth"
          _  -> show (SInt n) ++ "th"

-- Right, stop it! Started off with a nice little idea about grannies attacking young men, but now it's got silly.
-- (That is, it started out with 1,2,3 being exceptions but then look! more exceptions than the rule!)
-- (Oh, well.)

gifts :: [String]
gifts = reverse [ "Drummers Drumming"
                , "Pipers Piping"
                , "Lords-a-Leaping"
                , "Ladies Dancing"
                , "Maids-a-Milking"
                , "Swans-a-Swimming"
                , "Geese-a-Laying"
                , "Gold Rings"
                , "Calling Birds"
                , "French Hens"
                , "Turtle Doves"
                , "Partridge in a Pear Tree"
                ]

recite :: Int -> Int -> [String]
recite start stop = [verse n | n <- [start..stop]] where
  fmt = "On the %s day of Christmas my true love gave to me: %s."
  verse n = printf fmt (nth n) (intercalate ", " giftsOfVerse) where
    giftsOfVerse = reverse $ zipWith spellCount [1..n] (take n gifts)
    spellCount i = unwords . (count i :) . (:[])
    count i
      | i == 1 = if n == 1 then "a" else "and a"
      | otherwise = show (SInt i)

