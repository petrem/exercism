{-# LANGUAGE OverloadedStrings #-}
module Raindrops (convert) where

import Data.Maybe  (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)


-- A solution that attempts a bit of generalization.
{-
Assuming
- there could be multiple "rain words",
- the mapping from the input to "rain words" should be flexible,
- construction of the "rain phrase" is still an ordered accumulation of words,

-}


convert :: (Show a, Integral a) => a -> Text
convert n = undefined -- fromMaybe untranslatable $ foldMap (maybeWord n) rainDict
  where
    untranslatable = T.pack . show $ n

    maybeWord :: a -> b -> Maybe Text
    maybeWord = undefined
      
    divides _ 0 = False
    divides y x = x `rem` y == 0

    rainDict = undefined

mkWordTranslator :: (b -> a -> Bool) -> a -> b -> Maybe b
mkWordTranslator p from to = if p from to then Just to else Nothing

data RainTranslator a b = RainTranslator { runPred :: a -> Bool
                                         , getRain :: b
                                         }


data Droplet a b = Droplet a b deriving (Eq, Show)

type RainVocabulary t a b = t (a, b)

rainVocabulary :: RainVocabulary [] Int String
rainVocabulary = [(3, "Pling"), (5, "Plang"), (7, "Plong")]

-- toRainSpeak ::
--      (Foldable t, Integral a, Monoid b)
--   => (a -> Bool)
--   -> RainVocabulary t a b
--   -> a
--   -> Maybe b
-- toRainSpeak p ws n = foldMap (\(k, repr) -> if dividesBy k n then Just repr else Nothing) ws


{-
-- A simple, obvious version where the String monoid's `mempty` is flagging failure.

convert :: Int -> String
convert n = let converted = foldMap go [(3, "Pling"), (5, "Plang"), (7, "Plong")]
              in if null converted then show n else converted
  where go (k, repr) = if n `rem` k == 0 then repr else ""

-}


{-

-- Original solution that plays with IntMap. Not too useful.

import Data.IntMap (IntMap, fromList, (!), keys)
import Data.Maybe  (fromMaybe)

convert :: Int -> String
convert n = fromMaybe (show n) . mconcat $ map (rainSpeak n) (keys rainVocabulary)

rainSpeak :: Int -> Int -> Maybe String
rainSpeak n m = if n `mod` m == 0 then Just (rainVocabulary ! m) else Nothing

rainVocabulary :: IntMap String
rainVocabulary = fromList [(3, "Pling"), (5, "Plang"), (7, "Plong")]

-}

class Monoid m => MonoidRZero m where
  rzero :: m

data RainSpeak a = Thud | Pling a | Plang a | Plong a deriving (Eq, Show, Functor)

instance Semigroup (RainSpeak a) where
  x <> y =

instance MonoidRZero (RainSpeak a) where
  rzero = Thud
