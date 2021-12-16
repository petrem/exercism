module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where


import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Data.List as L
import qualified Data.Text as T

import Data.Maybe (fromJust)


data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

newtype Plants = Plants {unPlants :: [Plant]} deriving (Show)

instance Read Plants where
  readsPrec _ cs = [(Plants $ map readPlant cs, [])]

readPlant :: Char -> Plant
readPlant 'C' = Clover
readPlant 'G' = Grass
readPlant 'R' = Radishes
readPlant 'V' = Violets
readPlant _ = error "Unknown plant"


type Student = String


type Garden = Map Student Plants


garden :: [String] -> String -> Garden
garden students plants = Map.fromList $ zip students (map read . plantGroups $ plants)
  where
    plantGroups = map (T.unpack . T.concat) . L.transpose . map (T.chunksOf 2) . T.lines . T.pack

lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = unPlants . fromJust $ Map.lookup student garden
