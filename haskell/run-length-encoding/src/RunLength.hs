module RunLength (decode, encode) where

import Data.List (group, unfoldr)
import Data.Char (isNumber)


decode :: String -> String
decode = concat . unfoldr (f . span isNumber) where
  f ("","") = Nothing
  f ("", xs) = Just ([head xs], tail xs)
  f (n, xs) = Just (replicate (read n) . head $ xs, tail xs)


encode :: String -> String
encode = let lenPrefix x = if length x > 1 then show (length x) else "" in
  concatMap ( (++) <$> lenPrefix <*> (:[]) . head) . group
