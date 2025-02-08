module RationalNumbers
  ( Rational,
    abs,
    numerator,
    denominator,
    add,
    sub,
    mul,
    div,
    pow,
    expRational,
    expReal,
    rational,
  )
where

import Prelude hiding (Rational, abs, div)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Rational a = Rational !a !a deriving (Eq, Show)

rational :: (Integral a) => (a, a) -> Rational a
rational (n, d) =
  let gcdOfND = gcd n d
      sign = signum n * signum d
      reduce = (`quot` gcdOfND) . P.abs
   in Rational (sign * reduce n) (reduce d)

-- unary operators -------------------------------------------------------------
abs :: (Integral a) => Rational a -> Rational a
abs (Rational n d) = Rational (P.abs n) d

numerator :: (Integral a) => Rational a -> a
numerator (Rational n _) = n

denominator :: (Integral a) => Rational a -> a
denominator (Rational _ d) = d

-- binary operators ------------------------------------------------------------
add :: (Integral a) => Rational a -> Rational a -> Rational a
add (Rational a1 b1) (Rational a2 b2) = rational (a1 * b2 + a2 * b1, b1 * b2)

sub :: (Integral a) => Rational a -> Rational a -> Rational a
sub (Rational a1 b1) (Rational a2 b2) = rational (a1 * b2 - a2 * b1, b1 * b2)

mul :: (Integral a) => Rational a -> Rational a -> Rational a
mul (Rational a1 b1) (Rational a2 b2) = rational (a1 * a2, b1 * b2)

div :: (Integral a) => Rational a -> Rational a -> Rational a
div (Rational _ _) (Rational 0 _) = error "Cannot divide by 0"
div (Rational a1 b1) (Rational a2 b2) = rational (a1 * b2, a2 * b1)

pow :: (Integral a) => Rational a -> a -> Rational a
pow (Rational a b) n
  | n >= 0 = Rational (a ^ n) (b ^ n)
  | otherwise =
      let m = negate n
          sign = if even m then 1 else -1
       in Rational (sign * b ^ m) (P.abs a ^ m)

expRational :: (Integral a) => (Floating b) => Rational a -> b -> b
expRational (Rational a b) x = fromIntegral a ** x / fromIntegral b ** x

expReal :: (Floating a) => (Integral b) => a -> Rational b -> a
expReal x (Rational a b) = x ** (fromIntegral a / fromIntegral b)
