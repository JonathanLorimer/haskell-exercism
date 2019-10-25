module Triangle (TriangleType(..), triangleType) where

import Data.List (sort)

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
    |  (a == 0 || b == 0 || c == 0)
    || tIneq (sort [a, b, c])     = Illegal
    |  a == b && b == c           = Equilateral
    |  a == b || b == c || a == c = Isosceles
    |  otherwise                  = Scalene
  where
    tIneq [x, y, z] = x + y < z
    tIneq _         = error "impossible"

-- $> (\[x, y, z] -> x + y > z) [2, 3, 7]
