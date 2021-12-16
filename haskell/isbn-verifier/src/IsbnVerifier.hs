module IsbnVerifier (isbn) where

import qualified Data.Char as C
import Data.Monoid

isbn :: String -> Bool
isbn [] = False
isbn xs = ifAllTrue isbnChecks
  where
    isbnChecks = [all (\x -> C.isDigit x || x == '-') $ init xs
                 ,length digits == 9
                 ,C.isDigit (last xs) || C.toLower (last xs) == 'x'
                 ,checkSum `mod` 11 == 0
                 ]
    digits  = map C.digitToInt $ filter C.isDigit $ init xs
    control = (\x -> if C.isDigit x then C.digitToInt x else 10) $ last xs
    checkSum = sum $ zipWith (*) [1..10] (control:reverse digits)

ifAllTrue :: [Bool] -> Bool
ifAllTrue = getAll . mconcat . map All
