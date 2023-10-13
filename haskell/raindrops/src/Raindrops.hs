module Raindrops (convert) where

import Data.Maybe  (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)

convert :: Integral a => a -> Text
convert n = fromMaybe untranslatable (toRainSpeak raindropFromInt rainVocabulary n)
  where
    untranslatable = T.unpack . show $ n
    maybeDroplet rain = bool Nothing (Just rain) . dividesBy n
      
    dividesBy 0 _ = False
    dividesBy y x = x `rem` y == 0

    isDivisibleBy _ 0 = False
    isDivisibleBy x y = x `rem` y == 0

data Droplet a b = Droplet a b deriving (Eq, Show)

type RainVocabulary t a b = t (a, b)

rainVocabulary :: RainVocabulary [] Int String
rainVocabulary = [(3, "Pling"), (5, "Plang"), (7, "Plong")]

toRainSpeak ::
     (Foldable t, Integral a, Monoid b)
  => (a -> Bool)
  -> RainVocabulary t a b
  -> a
  -> Maybe b
toRainSpeak p ws n = foldMap (\(k, repr) -> if dividesBy k n then Just repr else Nothing) ws


{- A simple, obvious, version using String monoid

convert :: Int -> String
convert n = let converted = foldMap go [(3, "Pling"), (5, "Plang"), (7, "Plong")]
              in if null converted then show n else converted
  where go (k, repr) = if n `rem` k == 0 then repr else ""

-}


{- Original solution that plays with IntMap, but this isn't useful.

import Data.IntMap (IntMap, fromList, (!), keys)
import Data.Maybe  (fromMaybe)

convert :: Int -> String
convert n = fromMaybe (show n) . mconcat $ map (rainSpeak n) (keys rainVocabulary)

rainSpeak :: Int -> Int -> Maybe String
rainSpeak n m = if n `mod` m == 0 then Just (rainVocabulary ! m) else Nothing

rainVocabulary :: IntMap String
rainVocabulary = fromList [(3, "Pling"), (5, "Plang"), (7, "Plong")]

-}
