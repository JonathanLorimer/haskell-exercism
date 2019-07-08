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
             deriving (Eq, Show, Enum, Bounded)

data Robot = Robot { direction :: Bearing, x :: Integer, y :: Integer }

bearing :: Robot -> Bearing
bearing = direction

coordinates :: Robot -> (Integer, Integer)
coordinates robot = (x robot, y robot)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction (x, y) = Robot direction x y

move :: Robot -> String -> Robot
move robot [] = robot
move robot ('A':xs) = move (advance robot) xs
move robot (x:xs) = move (turn x robot) xs

advance :: Robot -> Robot
advance (Robot North x y)   = Robot North x (y + 1)
advance (Robot South x y)   = Robot South x (y - 1)
advance (Robot West x y)    = Robot West  (x - 1) y
advance (Robot East x y)    = Robot East  (x + 1) y

turn :: Char -> Robot -> Robot
turn 'L' (Robot dir x y) = Robot (safePred dir) x y
turn 'R' (Robot dir x y) = Robot (safeSucc dir) x y
turn _ robot = robot

safeSucc :: (Enum a, Bounded a, Eq a) => a -> a
safeSucc x
        | x == maxBound = minBound
        | otherwise = succ x

safePred :: (Enum a, Bounded a, Eq a) => a -> a
safePred x
        | x == minBound = maxBound
        | otherwise = pred x