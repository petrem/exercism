module ComplexNumbers
(Complex,
 conjugate,
 abs,
 exp,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs, exp)
import qualified Prelude as Pre

-- Data definition -------------------------------------------------------------
data Complex a = Complex a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (a, b) = Complex a b

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex a b) = Complex a (-b)

abs :: Floating a => Complex a -> a
abs (Complex a b) = sqrt (a**2 + b**2)

real :: Num a => Complex a -> a
real (Complex a _) = a

imaginary :: Num a => Complex a -> a
imaginary (Complex _ b) = b

exp :: Floating a => Complex a -> Complex a
exp (Complex a b) = Complex (Pre.exp a * cos b) (Pre.exp a * sin b)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a1 b1) (Complex a2 b2) = Complex (a1 * a2 - b1 * b2) (b1 * a2 + a1 * b2)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex a1 b1) (Complex a2 b2) = Complex (a1+a2) (b1+b2)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex a1 b1) (Complex a2 b2) = Complex (a1-a2) (b1-b2)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a1 b1) (Complex a2 b2) = Complex realPart imagPart
  where realPart = ((a1 * a2 + b1 * b2)/(a2*a2 + b2*b2))
        imagPart = ((b1 * a2 - a1 * b2)/(a2*a2 + b2*b2))
