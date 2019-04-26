module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify num
    | num <= 0 = Nothing
    | aSum < num = Just Deficient
    | aSum > num = Just Abundant
    | aSum == num = Just Perfect
    | otherwise = Nothing
        where
            aSum = aliquotSum num

numFactors :: Int -> [Int]
numFactors num = [x | x <- [1..num - 1], num `rem` x == 0]

aliquotSum :: Int -> Int
aliquotSum = sum . numFactors



