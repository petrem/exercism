module Pangram (isPangram) where

import Data.Char (toLower, isAlpha, toUpper, ord)
import Data.List (all, group, nub, sort)
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Exception
import Control.Monad
import System.Time.Extra

import Data.Bits

isPangram = isPangram5

isPangram1 :: String -> Bool
isPangram1 = (26 == ) . length . nub . sort . map toLower . filter isAlpha

-- getting rid of nub, need only to remove successive duplicates
isPangram2 :: String -> Bool
isPangram2 = (26 == ) . length . (map head . group) . sort . map toLower . filter isAlpha


-- likewise, using Sets
isPangram3 :: String -> Bool
isPangram3 = (26 == ) . Set.size . Set.fromList . map toLower . filter isAlpha

-- a more straight-forward approach
isPangram0 :: String -> Bool
isPangram0 xs = all (`elem` map toLower xs) alphabet
  where alphabet = ['a'..'z']

-- a classic recursive solution

isPangram_ :: Set Char -> String -> Bool
isPangram_ _ [] = False
isPangram_ s (x:xs)
  | not . isAlpha $ x = isPangram_ s xs
  | Set.size s' == 26 = True
  | otherwise = isPangram_ s' xs
  where s' = Set.insert (toLower x) s

isPangram4 :: String -> Bool
isPangram4 = isPangram_ Set.empty


-- bit field based, from max-min-median user
isPangram5 :: String -> Bool
isPangram5 text =
    let bitset = zeroBits :: Int
        h bs s 26 = True
        h bs "" count = False
        h bs (x:xs) count
            | ch < 0 || ch > 25 = h bs xs count
            | otherwise = h (setBit bs ch) xs (count + if testBit bs ch then 0 else 1)
            where ch = ord (toUpper x) - 65
    in h bitset text 0




-- benchmark functions, based on code in http://neilmitchell.blogspot.com/2015/02/nub-considered-harmful.html

benchmark xs = do
    n <- evaluate $ length xs
    (t1,_) <- duration $ evaluate $ isPangram xs
    (t2,_) <- duration $ evaluate $ isPangram3 xs
    putStrLn $ show n ++ "," ++ show t1 ++ "," ++ show t2

main = do
    forM_ [0,100..10000] $ \i -> benchmark $ take (26 * i) $ cycle ['a'..'z']
    forM_ [0,100..10000] $ \i -> benchmark $ replicate i 'a'
