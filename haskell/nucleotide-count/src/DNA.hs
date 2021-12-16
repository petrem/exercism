{-# Language TupleSections #-}
module DNA (nucleotideCounts, Nucleotide(..)) where

import Control.Applicative (liftA3)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Read (readEither)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Read, Show)
type NucleotideCounts = Map Nucleotide Int

-- solution #2

nucleotideCounts :: String -> Either String NucleotideCounts
nucleotideCounts = addMissingCounts . countNucleotides

addMissingCounts ::Either String NucleotideCounts -> Either String NucleotideCounts
addMissingCounts cs = Map.union <$> cs <*> Right zeroCounts

zeroCounts :: NucleotideCounts
zeroCounts = Map.fromList [(A,0), (C,0), (G,0), (T,0)]

countNucleotides :: String -> Either String NucleotideCounts
countNucleotides xs = Map.fromListWith (+) . fmap (,1) <$> readNucleotides xs

readNucleotides :: String -> Either String [Nucleotide]
readNucleotides = traverse (readEither . (:[]))

-- solution #1

nucleotideCounts' :: String -> Either String NucleotideCounts
nucleotideCounts' = foldl updateCounts (Right zeroCounts)

updateCounts :: Either String NucleotideCounts -> Char -> Either String NucleotideCounts
updateCounts m c = liftA3 (Map.insertWith (+)) (readEither [c]) (Right 1) m
