-- "at least it's working ;-)"
-- I wonder where I should start improving this...
module TwoBucket where

import Data.Foldable (asum)
import Data.List (find, unfoldr)
import Data.Set (Set)
import qualified Data.Set as Set

type Volume = Int

type Capacities = (Volume, Volume)

type State = (Volume, Volume)

type Steps = Int

measure :: Capacities -> Volume -> Maybe (Steps, State)
measure capacities targetVolume = (,) <$> pathSteps <*> pathState <$> asum (checkForTarget <$> generations)
  where
    seed = Frontier [startPath] startSeen
    startPath = extendPath capacities (Path 0 emptyState) FillFirst
    startSeen = Set.fromList [emptyState, pathState startPath, invalidState]
    emptyState = (0, 0)
    -- constraint: "you may not arrive at a state where the initial starting bucket
    -- is empty and the other bucket is full."
    -- so we pretend we've seen this state
    invalidState = pathState $ extendPath capacities (Path 0 emptyState) FillSecond

    checkForTarget = find (isGoalReached targetVolume . pathState)
    generations = unfoldr (newFrontier' capacities) seed

isGoalReached :: Volume -> State -> Bool
isGoalReached target (v1, v2) = v1 == target || v2 == target

data Action
  = EmptyFirst
  | EmptySecond
  | FillFirst
  | FillSecond
  | PourFirstToSecond
  | PourSecondToFirst
  deriving (Bounded, Enum, Show)

actions :: [Action]
actions = enumFrom minBound

perform :: Capacities -> Action -> State -> State
perform _ EmptyFirst (_, v2) = (0, v2)
perform _ EmptySecond (v1, _) = (v1, 0)
perform (c1, _) FillFirst (_, v2) = (c1, v2)
perform (_, c2) FillSecond (v1, _) = (v1, c2)
perform (_, c2) PourFirstToSecond (v1, v2) = (v1 - amount, v2 + amount)
  where
    amount = min v1 (c2 - v2)
perform (c1, _) PourSecondToFirst (v1, v2) = (v1 + amount, v2 - amount)
  where
    amount = min v2 (c1 - v1)

-- Summary of a path through the state space
data Path = Path
  { pathSteps :: Steps,
    pathState :: State
  }
  deriving (Show)

extendPath :: Capacities -> Path -> Action -> Path
extendPath c (Path steps state) a = Path (steps + 1) (perform c a state)

data Frontier = Frontier [Path] (Set State) deriving (Show)

-- Find the new "frontier" in the state space from a given list of paths
newFrontier :: Capacities -> Frontier -> Maybe Frontier
newFrontier c (Frontier ps seen) =
  if null newPaths
    then Nothing
    else Just $ Frontier newPaths newSeen
  where
    newPaths =
      [ extendPath c p a
        | p <- ps,
          a <- actions,
          pathState (extendPath c p a) `Set.notMember` seen
      ]
    newSeen = Set.union seen (Set.fromList $ map pathState newPaths)

newFrontier' :: Capacities -> Frontier -> Maybe ([Path], Frontier)
newFrontier' c (Frontier ps seen) =
  if null newPaths
    then Nothing
    else Just (ps, Frontier newPaths newSeen)
  where
    newPaths =
      [ extendPath c p a
        | p <- ps,
          a <- actions,
          pathState (extendPath c p a) `Set.notMember` seen
      ]
    newSeen = Set.union seen (Set.fromList $ map pathState newPaths)
