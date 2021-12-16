module Cipher
  ( caesarDecode
  , caesarEncode
  , caesarEncodeRandom
  ) where

import qualified Data.Char as C
import System.Random


caesarDecode :: String -> String -> String
caesarDecode key = zipWith decodeChar (cycle key)

caesarEncode :: String -> String -> String
caesarEncode key = zipWith encodeChar (cycle key)

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  seed <- newRand
  let key = genRandomKey seed 100
  return (key, caesarEncode key text)

decodeChar :: Char -> Char -> Char
decodeChar k c = undelta $ (delta c - delta k) `mod` alphabetSize

encodeChar :: Char -> Char -> Char
encodeChar k c = undelta $ (delta c + delta k) `mod` alphabetSize

alphabet :: [Char]
alphabet = ['a'..'z']

alphabetSize :: Int
alphabetSize = length alphabet

delta :: Char -> Int
delta c = C.ord c - C.ord 'a'

undelta :: Int -> Char
undelta i = C.chr $ i + C.ord 'a'

newRand :: IO Int
newRand = randomIO

-- seed -> random list
randomAlphabetIndexes :: Int -> [Int]
randomAlphabetIndexes = map ((`mod` alphabetSize) . abs) . randoms . mkStdGen

genRandomKey :: Int -> Int -> String
genRandomKey seed size = map (alphabet !!) $ take size (randomAlphabetIndexes seed)
