module ResistorColors (Color(..), value) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Enum, Show, Read)

value :: [Color] -> Int
value = read . concatMap (show . fromEnum)
