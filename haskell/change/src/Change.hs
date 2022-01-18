--module Change (findFewestCoins) where
module Change where

import Control.Monad (join)
import Control.Monad.State (State, get, put, evalState)
import Data.Sequence (Seq, singleton, (!?), (|>))
import Data.Foldable (minimumBy)
import Data.Maybe (mapMaybe)


type Coin = Integer
type Denominations = [Coin]
type Change = [Coin]
type MoneyAsChange = Maybe Change



{-

To Do:
- replace 'foldl1' with 'foldl'
- surely there's a more idiomatic way to express that @foldl (>>) (map ...)@ part
- could ignore the state value, use 'execState', and use 'replicateM' -- but then 'State' wouldn't seem like the appropriate abstraction
- could probably use a 'Reader' to avoid passing the list of denominations around, revisit after I look into composing types and monad transformers?

Changes:
- switched from DList (dlist) to Seq in containers as Exercism doesn't find the former. Fingers crossed for the latter.
-}

findFewestCoins :: Integer -> Denominations -> MoneyAsChange
findFewestCoins target denominations
  | target < 0  = Nothing
  | target == 0 = Just []
  | otherwise   = evalState (changeListUpTo denominations target) changeZero


changeZero :: Seq MoneyAsChange
changeZero = singleton $ Just []


changeListUpTo :: Denominations -> Integer -> State (Seq MoneyAsChange) MoneyAsChange
changeListUpTo denominations upTo = foldl1 (>>) (map (addSolution denominations) [1..upTo])


addSolution :: Denominations -> Integer -> State (Seq MoneyAsChange) MoneyAsChange
addSolution denominations target = do
  solutions <- get
  let usableCoins = filter (<= target) denominations
      changeVariants =
        -- solutions !? target -c :: MoneyAsChange ~ Maybe [Integer]
        mapMaybe (\c -> (c:) <$> join (solutions !? fromIntegral (target-c))) usableCoins
      change = if null changeVariants
               then Nothing
               else Just $ minimumBy compareByLength changeVariants
  put $ solutions |> change
  return change


(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

compareByLength :: (Ord a, Foldable t) => t a -> t a -> Ordering
compareByLength a b = compare (length a) (length b)


-- recursive solution (takes forever)

{-

findFewestCoins :: Integer -> Denominations -> Maybe Change
findFewestCoins target coins
  | target < 0  = Nothing
  | target == 0 = Just []
  | otherwise   = case solutions of
                    [] -> Nothing
                    _ -> let (coin, solution) = minimumBy compareSndByLength solutions
                         in Just $ coin:solution
  where usableCoins = filter (<= target) coins
        solutions = mapMaybe (sequenceA . applyToSnd ((`findFewestCoins` coins) . (target -))) usableCoins


compareSndByLength :: (Ord b, Foldable t) => (a, t b) -> (a, t b) -> Ordering
compareSndByLength (_, b1) (_, b2) = compare (length b1) (length b2)

applyToSnd:: (a -> b) -> a -> (a, b)
applyToSnd f a = (a, f a)

-}


-- a dynamic-programming-ish version, using State
-- I don't think it really worked

{-

findFewestCoins :: Integer -> Denominations -> Maybe Change
findFewestCoins target coins = fst (replicateM (fromInteger target) $ runState changeState (coins, []))


changeState :: State (Denominations, DL.DList (Maybe Change)) (Maybe Change)
changeState = do
  (coins, solutions) <- get
  if null solutions
    then do
        put (coins, [Just []])  -- initialize for 0
        return $ Just []
    else do
      let n = fromIntegral $ length solutions -- current target sum to break down into coins
          usableCoins = filter (<= n) coins
          solutionsForN = map (\c -> (c:) <$> (solutions !! fromIntegral (n-c))) usableCoins
          solutionForN = if null solutionsForN
                         then Nothing
                         else minimumBy compareByLength solutionsForN
      put (coins, solutions ++ [solutionForN])
      return solutionForN

-}
