module IsbnVerifier
  ( isbn
  ) where

import qualified Data.Char as C

isbn :: String -> Bool
isbn xs =
  maybe False ((&&) <$> checksumIsValid . fst <*> (== 11) . snd) (isbn10Checksum xs)
  where
    checksumIsValid = (== 0) . (`mod` 11)

isbn10Checksum :: String -> Maybe (Int, Int)
isbn10Checksum = foldr go (Just (0, 1))
  where
    go _ Nothing = Nothing
    go digit (Just (checksum, multiplier))
      | multiplier > 10 = Nothing
      | C.isDigit digit =
        Just (checksum + multiplier * C.digitToInt digit, multiplier + 1)
      | digit == '-' = Just (checksum, multiplier)
      | C.toLower digit == 'x' && multiplier == 1 = Just (checksum + 10, 2)
      | otherwise = Nothing
