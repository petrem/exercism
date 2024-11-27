module ZebraPuzzle (Resident (..), Solution (..), solve) where

import Data.List (permutations, zipWith5)

data Resident
  = Englishman
  | Spaniard
  | Ukrainian
  | Norwegian
  | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Solution = Solution
  { waterDrinker :: Resident,
    zebraOwner :: Resident
  }
  deriving (Eq, Show)

solve :: Solution
solve =
  Solution
    { waterDrinker = residentWho ((Water ==) . drink) solution,
      zebraOwner = residentWho ((Zebra ==) . pet) solution
    }
  where
{- ORMOLU_DISABLE -}
    solution = head $ [
      zipWith5 House rs cs ps ds hs | rs <- permutations (enumerate::[Resident])
                                    , rule10 rs
                                    , cs <- permutations (enumerate::[Color])
                                    , rule6  cs
                                    , rule2  cs rs
                                    , rule15 cs rs
                                    , ds <- permutations (enumerate::[Drink])
                                    , rule9  ds
                                    , rule5  rs ds
                                    , rule4  cs ds
                                    , hs <- permutations (enumerate::[Hobby])
                                    , rule8  cs hs
                                    , rule13 ds hs
                                    , rule14 rs hs
                                    , ps <- permutations (enumerate::[Pet])
                                    , rule3  rs ps
                                    , rule7  ps hs
                                    , rule11 ps hs
                                    , rule12 ps hs
                                    ]
{- ORMOLU_ENABLE -}

data House = House
  { resident :: Resident,
    color :: Color,
    pet :: Pet,
    drink :: Drink,
    hobby :: Hobby
  }
  deriving (Eq, Show)

data Color = Red | Green | Ivory | Yellow | Blue deriving (Eq, Show, Enum, Bounded)

data Pet = Dog | Snail | Fox | Horse | Zebra deriving (Eq, Show, Enum, Bounded)

data Drink = Coffee | Tea | Milk | Juice | Water deriving (Eq, Show, Enum, Bounded)

data Hobby = Dancing | Paint | Reading | Chess | Football deriving (Eq, Show, Enum, Bounded)

-- Rule 1: # Rule 1: There are five houses.

-- | The Englishman lives in the red house.
rule2 :: [Color] -> [Resident] -> Bool
rule2 = checkPaired Red Englishman

-- | The Spaniard owns the dog.
rule3 :: [Resident] -> [Pet] -> Bool
rule3 = checkPaired Spaniard Dog

-- | The person in the green house drinks coffee.
rule4 :: [Color] -> [Drink] -> Bool
rule4 = checkPaired Green Coffee

-- | The Ukrainian drinks tea.
rule5 :: [Resident] -> [Drink] -> Bool
rule5 = checkPaired Ukrainian Tea

-- | The green house is immediately to the right of the ivory house.
rule6 :: [Color] -> Bool
-- Red is some initial value on the right that isn't Green. I do this differently in other rules.
rule6 = fst . foldr (\cur (rez, prev) -> (rez || prev == Green && cur == Ivory, cur)) (False, Red)

-- | The snail owner likes to go dancing.
rule7 :: [Pet] -> [Hobby] -> Bool
rule7 = checkPaired Snail Dancing

-- | The person in the yellow house is a painter.
rule8 :: [Color] -> [Hobby] -> Bool
rule8 = checkPaired Yellow Paint

-- | The person in the middle house drinks milk.
rule9 :: [Drink] -> Bool
rule9 ds = ds !! 2 == Milk

-- | The Norwegian lives in the first house.
rule10 :: [Resident] -> Bool
rule10 rs = rs !! 0 == Norwegian {- HLINT ignore "Use head" -}

-- | The person who enjoys reading lives in the house next to the person with the fox.
rule11 :: [Pet] -> [Hobby] -> Bool
rule11 = checkNext Fox Reading

-- | The painter's house is next to the house with the horse.
rule12 :: [Pet] -> [Hobby] -> Bool
rule12 = checkNext Horse Paint

-- | The person who plays football drinks orange juice.
rule13 :: [Drink] -> [Hobby] -> Bool
rule13 = checkPaired Juice Football

-- | The Japanese person plays chess.
rule14 :: [Resident] -> [Hobby] -> Bool
rule14 = checkPaired Japanese Chess

-- | The Norwegian lives next to the blue house.
rule15 :: [Color] -> [Resident] -> Bool
rule15 = checkNext Blue Norwegian

checkPaired :: (Eq a, Eq b) => a -> b -> [a] -> [b] -> Bool
checkPaired a b as bs = (a, b) `elem` zip as bs

checkNext :: (Eq a, Eq b) => a -> b -> [a] -> [b] -> Bool
checkNext a b as bs = (a, b) `elem` zip as (tail bs) ++ zip (tail as) bs

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

residentWho :: (House -> Bool) -> [House] -> Resident
residentWho f = resident . head . filter f
