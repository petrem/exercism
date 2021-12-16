module Raindrops (convert) where

import Data.IntMap (IntMap, fromList, (!), keys)
import Data.Maybe  (fromMaybe)
import Data.Monoid (mconcat)


convert :: Int -> String
convert n = fromMaybe (show n) . mconcat $ map (rainSpeak n) (keys rainVocabulary)

rainSpeak :: Int -> Int -> Maybe String
rainSpeak n m = if n `mod` m == 0 then Just (rainVocabulary ! m) else Nothing

rainVocabulary :: IntMap String
rainVocabulary = fromList [(3, "Pling"), (5, "Plang"), (7, "Plong")]
