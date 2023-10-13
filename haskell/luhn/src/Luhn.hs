module Luhn
  ( isValid
  ) where

import Control.Monad (foldM)
import Data.Char (digitToInt, isDigit)

-- This solution parses the string a single time.
-- Perhaps a more elegant way could be devised using Text and its built-in fusion?
isValid :: String -> Bool
isValid = maybe False ((&&) <$> dividesBy 10 . fst <*> (> 1) . snd) . luhnChecksumMaybe
  where
    dividesBy x = (0 ==) . (`mod` x)

luhnChecksumMaybe :: String -> Maybe (Int, Int)
luhnChecksumMaybe = (fstSnd3 <$>) . foldM go (0, 0, False) . reverse
  where
    go acc@(x, count, flipflop) c
      | c == ' ' = Just acc
      | isDigit c =
        Just (x + luhnDouble flipflop (digitToInt c), count + 1, not flipflop)
      | otherwise = Nothing
    luhnDouble True n =
      let nn = n * 2
       in if nn > 9
            then nn - 9
            else nn
    luhnDouble False n = n
    fstSnd3 (a, b, _) = (a, b)

data LuhnSum = LuhnError String | LuhnSum { checksum :: Int
                                          , idLength :: Int
                                          } deriving (Eq, Show)

instance ...
