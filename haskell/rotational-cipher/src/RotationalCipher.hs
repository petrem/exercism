module RotationalCipher (rotate) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

rotate :: Int -> String -> String
rotate key = map (mkTranslator [['A' .. 'Z'], ['a' .. 'z']] rot)
  where
    rot = take 26 . drop key . cycle

mkTranslator :: [[Char]] -> ([Char] -> [Char]) -> Char -> Char
mkTranslator charSets mapper = \c ->
  fromMaybe c . Map.lookup c . Map.fromList . (zip <$> concat <*> (mapper =<<)) $
    charSets
