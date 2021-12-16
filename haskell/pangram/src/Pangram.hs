module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)
import Data.List (all, group, nub, sort)
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Exception
import Control.Monad
import System.Time.Extra


isPangram1 :: String -> Bool
isPangram1 = (26 == ) . length . nub . sort . map toLower . filter isAlpha

-- getting rid of nub, need only to remove successive duplicates
isPangram2 :: String -> Bool
isPangram2 = (26 == ) . length . (map head . group) . sort . map toLower . filter isAlpha


-- likewise, using Sets
isPangram3 :: String -> Bool
isPangram3 = (26 == ) . Set.size . Set.fromList . map toLower . filter isAlpha

-- a more straight-forward approach
isPangram :: String -> Bool
isPangram xs = all (`elem` map toLower xs) alphabet
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


-- benchmark functions, based on code in http://neilmitchell.blogspot.com/2015/02/nub-considered-harmful.html

benchmark xs = do
    n <- evaluate $ length xs
    (t1,_) <- duration $ evaluate $ isPangram xs
    (t2,_) <- duration $ evaluate $ isPangram3 xs
    putStrLn $ show n ++ "," ++ show t1 ++ "," ++ show t2

main = do
    forM_ [0,100..10000] $ \i -> benchmark $ take (26 * i) $ cycle ['a'..'z']
    forM_ [0,100..10000] $ \i -> benchmark $ replicate i 'a'
