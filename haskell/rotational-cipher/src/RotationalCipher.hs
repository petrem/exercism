module RotationalCipher (rotate) where

-- import Data.Char (chr, ord, isAsciiLower, isAsciiUpper)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

rotate :: Int -> String -> String
rotate key = map translate
  where
    translate = mkTrans asciiLetters (rot repeatedLowers ++ rot repeatedUppers)
    rot = take 26 . drop key

repeatedLowers = ['a' .. 'z'] ++ ['a' .. 'z']

repeatedUppers = ['A' .. 'Z'] ++ ['A' .. 'Z']

asciiLetters = ['a' .. 'z'] ++ ['A' .. 'Z']

mkTrans :: [Char] -> [Char] -> Char -> Char
mkTrans from to = \c -> fromMaybe c (Map.lookup c charMap)
  where
    charMap = Map.fromList $ zip from to
