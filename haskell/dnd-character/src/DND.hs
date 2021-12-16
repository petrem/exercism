module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Control.Monad (ap)
import Data.List
import Test.QuickCheck (Gen, choose)

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier x = (x - 10) `div` 2

ability :: Gen Int
ability = sum . drop 1 . sort <$> rollDice 6 4

character' :: Gen Character
character' = do
  a1 <- ability
  a2 <- ability
  a3 <- ability
  a4 <- ability
  a5 <- ability
  a6 <- ability
  return (Character a1 a2 a3 a4 a5 a6 (10 + modifier a3))

character :: Gen Character
character = return (\a1 a2 a3 a4 a5 a6 -> Character a1 a2 a3 a4 a5 a6 (10 + modifier a3)) `ap` ability `ap` ability `ap` ability `ap` ability `ap` ability `ap` ability


rollDice :: Int -> Int -> Gen [Int]
rollDice faces times = sequence [choose (1, faces) | _ <- [1..times]]
