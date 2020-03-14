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

import           Prelude hiding (abs, div, exp)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Complex a = a :+ a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (x, y) = x :+ y

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (x :+ y) = x :+ (-y)

abs :: Floating a => Complex a -> a
abs (x :+ y) = sqrt(x^2 + y^2)

real :: Num a => Complex a -> a
real (x :+ _) = x

imaginary :: Num a => Complex a -> a
imaginary (_ :+ y) = y

exp :: Floating a => Complex a -> Complex a
exp (x :+ y) =  ((P.exp x) * cos y) :+ ((P.exp x) * sin y)
-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (x :+ y) (x' :+ y') = (x * x' - y * y') :+ (x' * y + x * y')

add :: Num a => Complex a -> Complex a -> Complex a
add (x :+ y) (x' :+ y') = (x + x') :+ (y + y')

sub :: Num a => Complex a -> Complex a -> Complex a
sub (x :+ y) (x' :+ y') = (x - x') :+ (y - y')

div :: Fractional a => Complex a -> Complex a -> Complex a
div (a :+ b) (c :+ d) = ((a * c + b * d)/(c^2 + d^2)) :+ ((b * c - a * d)/(c^2 + d^2))
