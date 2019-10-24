module Yacht (yacht, Category(..)) where

import Data.List (foldl', group, sort)
import Data.Bool (bool)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht
                deriving (Enum, Eq, Ord, Show)
accNum :: Int -> Int -> Int -> Int
accNum match acc a = if match == a then acc + match else acc

checkGroup :: ([Int] -> Bool) -> ([[Int]] -> Int) -> [Int] -> Int
checkGroup p f l =
  let g = group . sort $ l
  in if p . fmap length $ g
      then f g
      else 0

yacht :: Category -> [Int] -> Int
yacht Yacht           = bool 0 50 . \(x:xs) -> all (==x) xs
yacht Choice          = sum
yacht BigStraight     = bool 0 30 . (== [2,3,4,5,6]) . sort
yacht LittleStraight  = bool 0 30 . (== [1,2,3,4,5]) . sort
yacht FourOfAKind     = checkGroup ((== 1) . length . filter (4 <=)) (sum . take 4 . maximum)
yacht FullHouse       = checkGroup (\x -> elem 2 x && elem 3 x) (sum . concat)
yacht cat             = foldl' (accNum (fromEnum cat + 1)) 0

