{-# LANGUAGE TupleSections #-}

module ETL (transform) where

import           Data.Bifunctor (bimap)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Tuple (swap)

transform :: Map Int Text -> Map Char Int
transform = M.fromList . concatMap (uncurry zip . swap . bimap repeat (T.unpack . T.toLower)) . M.toList

transform' :: Map Int Text -> Map Char Int
transform'  = M.fromList . concatMap (\(x, ys) -> [(y, x) | y <- T.unpack . T.toLower $ ys]) . M.toList
