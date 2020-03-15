module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.List
import Data.Maybe

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum, Ord, Bounded)

allergens :: [Allergen]
allergens = [minBound..maxBound]

scores :: [Int]
scores = (2^) . fromEnum <$> allergens

assocScores :: [(Int, Allergen)]
assocScores = scores `zip` allergens

allergies :: Int -> [Allergen]
allergies s = sort $ go s
  where
      go 0 = []
      go score = fromJust (lookup straddle assocScores) : allergies (score - straddle)

      straddle = findStraddle s scores

      findStraddle :: Int -> [Int] -> Int
      findStraddle _ [] = error "shouldn't be called with empty list"
      findStraddle _ [x] = x
      findStraddle n (x:y:ys) = if n >= x && n < y
                                  then x
                                  else findStraddle n (y:ys)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = allergen `elem` allergies score
