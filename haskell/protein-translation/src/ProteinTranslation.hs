module ProteinTranslation(proteins) where

import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)

data Codon = AUG | UUU | UUC | UUA | UUG | UCU | UCC | UCA | UCG | UAU | UAC | UGU | UGC
           | UGG | UAA  | UAG | UGA
           deriving (Eq, Ord, Show, Read)

data Protein = Methionine
             | Phenylalanine
             | Leucine
             | Serine
             | Tyrosine
             | Cysteine
             | Tryptophan
             | Stop
             deriving (Eq, Show, Read)

proteinCodons :: [(Protein, [Codon])]
proteinCodons = [(Methionine, [AUG])
                ,(Phenylalanine, [UUU, UUC])
                ,(Leucine, [UUA, UUG])
                ,(Serine, [UCU, UCC, UCA, UCG])
                ,(Tyrosine, [UAU, UAC])
                ,(Cysteine, [UGU, UGC])
                ,(Tryptophan, [UGG])
                ,(Stop, [UAA, UAG, UGA])]

proteins :: String -> Maybe [String]
proteins xs = map show . takeWhile (/= Stop) <$> proteinsMaybe xs

proteinsMaybe :: String -> Maybe [Protein]
proteinsMaybe xs = mapM (>>= codonToProtein) (parseCodons xs)

parseCodons :: String -> [Maybe Codon]
parseCodons = map readMaybe . chunksOf 3

codonToProtein :: Codon -> Maybe Protein
codonToProtein = flip Map.lookup codonsToProteinsMap

codonsToProteinsMap :: Map Codon Protein
codonsToProteinsMap = Map.fromList [(y,x) | (x,ys) <- proteinCodons, y <- ys]
