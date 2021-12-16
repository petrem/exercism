{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module ResistorColors
  ( Color(..)
  , Resistor(..)
  , label
  , ohms
  ) where


import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T


data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

valueAndZeroes :: Resistor -> (Int, Int)
valueAndZeroes r =
  let (d1, d2, z) = bands r
   in case (d1, d2) of
        (Black, Black) -> (0, 0)
        (_, Black) -> (fromEnum d1, fromEnum z + 1)
        _ -> (fromEnum d1 * 10 + fromEnum d2, fromEnum z)

label :: Resistor -> Text
label resistor =
  let (value, unit, leftZeroes) = prefixed $ valueAndZeroes resistor
   in T.concat
        [ T.pack $ show value
        , T.replicate (fromIntegral leftZeroes) "0"
        , " "
        , unit
        , "ohms"
        ]

ohms :: Resistor -> Int
ohms r = let (v, z) = valueAndZeroes r
         in 10 ^ z * v

unitPrefixes :: [Text]
unitPrefixes = ["", "kilo", "mega", "giga"]

unitAndReminder :: Int -> (Text, Int)
unitAndReminder zeroes
  | zeroes < 0  = error "Why so negative?"
  | otherwise =
      let (mult, rest) = divMod zeroes 3
      in (unitPrefixes !! mult, rest)

prefixed :: (Int, Int) -> (Int, Text, Int)
prefixed (v, z) = uncurry (v,,) $ unitAndReminder z
