module Series (Error(..), largestProduct) where

import Data.Char (digitToInt)
import Data.List (tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits = findLargestProduct =<< traverse eitherDigitToInteger digits
  where
    findLargestProduct xs | length xs < size = Left InvalidSpan
                          | size < 0 = Left InvalidSpan
                          | size == 0 = Right 1
                          | null xs = Left InvalidSpan
                          | otherwise = Right . maximum . products $ xs
    products = map product . filter ((==size) . length) . map (take size) . tails

eitherDigitToInteger :: Char -> Either Error Integer
eitherDigitToInteger x | x `elem` ['0'..'9'] = Right . toInteger . digitToInt $ x
                       | otherwise = Left . InvalidDigit $ x

