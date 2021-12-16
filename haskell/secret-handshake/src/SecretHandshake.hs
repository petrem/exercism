module SecretHandshake (handshake) where

import Data.Bits (testBit)


bitOps :: [(Int, [String] -> [String])]
bitOps = [ (4, reverse)
         , (0, ("wink":))
         , (1, ("double blink":))
         , (2, ("close your eyes":))
         , (3, ("jump":))
         ]


handshake :: Int -> [String]
handshake n = foldr (\ (bit, op) acc -> if testBit n bit then op acc else acc) [] bitOps
