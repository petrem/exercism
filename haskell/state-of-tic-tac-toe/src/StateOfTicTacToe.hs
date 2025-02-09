module StateOfTicTacToe (gameState, GameState (..)) where

import Data.List (transpose)

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)

gameState :: [String] -> GameState
gameState board =
  checkTotalCounts totals <> mconcat (lineState <$> lineCounts)
  where
    lineCounts = squaresCount <$> boardLines board
    totals = mconcat . fmap squaresCount $ board

data SquareCounts = SquareCounts !Int !Int !Int deriving (Eq, Show)

instance Semigroup SquareCounts where
  (SquareCounts e1 x1 o1) <> (SquareCounts e2 x2 o2) =
    SquareCounts (e1 + e2) (x1 + x2) (o1 + o2)

instance Monoid SquareCounts where
  mempty = SquareCounts 0 0 0

squareCount :: Char -> SquareCounts
squareCount ' ' = SquareCounts 1 0 0
squareCount 'X' = SquareCounts 0 1 0
squareCount 'O' = SquareCounts 0 0 1
squareCount _ = error "invalid square"

squaresCount :: String -> SquareCounts
squaresCount = foldMap squareCount

boardLines :: [String] -> [String]
boardLines xss = xss ++ transpose xss ++ [zipWith (!!) xss [0, 1, 2], zipWith (!!) xss [2, 1, 0]]

lineState :: SquareCounts -> GameState
lineState (SquareCounts e x o)
  | x == 3 = WinX
  | o == 3 = WinO
  | x + o + e == 3 = Ongoing
  | otherwise = Impossible

instance Semigroup GameState where
  Ongoing <> x = x
  x <> Ongoing = x
  WinX <> Draw = WinX
  WinX <> WinX = WinX
  WinX <> _ = Impossible
  WinO <> Draw = WinO
  WinO <> WinO = WinO
  WinO <> _ = Impossible
  Impossible <> _ = Impossible
  _ <> Impossible = Impossible
  Draw <> WinX = WinX -- ?
  Draw <> WinO = WinO -- ?
  Draw <> Draw = Draw

instance Monoid GameState where
  mempty = Ongoing

checkTotalCounts :: SquareCounts -> GameState
checkTotalCounts (SquareCounts e x o)
  | e + x + o /= 9 = Impossible
  | x - o /= 0 && x - o /= 1 = Impossible
  | x + o == 9 = Draw
  | otherwise = Ongoing
