module Triangle (TriangleType(..), triangleType) where

data TriangleType = Scalene
                  | Isosceles
                  | Equilateral
                  | Illegal
                  deriving (Eq, Ord, Show)

instance Semigroup TriangleType where
  Isosceles <> Isosceles = Equilateral
  x <> y = if x >= y then x else y

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c = isTriangle a b c <> isoscelesFrom a b <> isoscelesFrom b c <> isoscelesFrom a c

isTriangle ::  (Num a, Ord a) => a -> a -> a -> TriangleType
isTriangle a b c
  | any (<= 0) [a,b,c] = Illegal
  | a + b < c          = Illegal
  | a + c < b          = Illegal
  | c + b < a          = Illegal
  | otherwise          = Scalene

isoscelesFrom :: (Num a, Ord a) => a -> a -> TriangleType
isoscelesFrom x y
  | x == y    = Isosceles
  | otherwise = Scalene
