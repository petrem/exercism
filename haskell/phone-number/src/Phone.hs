module Phone (number) where

import qualified Data.Char as C
import Control.Applicative


maybeDigit :: Int -> Int -> Char -> Maybe Char
maybeDigit from to c
  | C.isDigit c = if (&&) <$> (>= from) <*> (<= to) $ C.digitToInt c then Just c else Nothing
  | otherwise   = Nothing


phoneNumberFormat :: [Char -> Maybe Char]
phoneNumberFormat = [maybeN, maybeX, maybeX, maybeN] ++ replicate 6 maybeX
  where maybeX = maybeDigit 0 9
        maybeN = maybeDigit 2 9

phoneNumberFormatWithCountry :: [Char -> Maybe Char]
phoneNumberFormatWithCountry = maybeOne : phoneNumberFormat
  where maybeOne = maybeDigit 1 1

checkNumber :: [Char -> Maybe Char] -> String -> Maybe String
checkNumber format xs = sequenceA . getZipList $ ZipList format <*> ZipList xs

number :: String -> Maybe String
number xs = do
  let onlyDigits = filter C.isDigit xs
  case length onlyDigits of
    11 -> tail <$> checkNumber phoneNumberFormatWithCountry onlyDigits
    10 -> checkNumber phoneNumberFormat onlyDigits
    _  -> Nothing
