module Strain (keep, discard) where

import Data.Maybe

discard :: (a -> Bool) -> [a] -> [a]
discard p = mapMaybe ensure
  where ensure x
          | p x       = Nothing
          | otherwise = Just x

keep :: (a -> Bool) -> [a] -> [a]
keep p = foldr (\x acc -> if p x then x:acc else acc) []
