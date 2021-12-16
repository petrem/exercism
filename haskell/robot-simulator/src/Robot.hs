module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Bounded, Enum)

newtype Coordinates = Coordinates { fromCoordinates :: (Integer, Integer)}

instance Semigroup Coordinates where
  Coordinates (x1, y1) <> Coordinates (x2, y2) = Coordinates (x1+x2, y1+y2)

fromBearing :: Bearing -> Coordinates
fromBearing East  = Coordinates ( 1,  0)
fromBearing West  = Coordinates (-1,  0)
fromBearing North = Coordinates ( 0,  1)
fromBearing South = Coordinates ( 0, -1)


-- <Stolen from="https://stackoverflow.com/a/5684147">

next :: (Enum a, Bounded a) => a -> a
next = turn 1

prev :: (Enum a, Bounded a) => a -> a
prev = turn (-1)

turn :: (Enum a, Bounded a) => Int -> a -> a
turn n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
    where
      add modulo x y = (x + y + modulo) `rem` modulo

-- </Stolen>

data Robot = Robot { bearing :: Bearing
                   , coordinates :: (Integer, Integer)
                   }

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move = foldl step where
  step (Robot b c) i = case i of
                      'L' -> Robot (prev b) c
                      'R' -> Robot (next b) c
                      'A' -> Robot b $ fromCoordinates (Coordinates c <> fromBearing b)
                      _   -> error ("Unknown instruction: " ++ show i)
