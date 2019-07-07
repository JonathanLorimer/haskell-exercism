module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)
import Data.Bifunctor (bimap)

isbn :: String -> Bool
isbn str = do
    let split = splitAt 9 . filter (/= '-') $ str
    if isInvalidFormat split
    then False
    else dividesByEleven . sum . (\(x,y) -> y : x) . (bimap handleBody handleLast) $ split

isInvalidFormat :: (String, String) -> Bool
isInvalidFormat split =  ((snd split) /= "X" && notNumber (snd split)) 
                      || 1 /= length (snd split)
                      || any (not . isDigit) (fst split)

handleBody :: String -> [Int]
handleBody = zipWith (*) [10,9..2] . fmap digitToInt

handleLast :: String -> Int
handleLast "X"      = 10
handleLast x        = read x

dividesByEleven :: Int -> Bool
dividesByEleven x = x `mod` 11 == 0

notNumber :: String -> Bool
notNumber [x] = not . isDigit $ x
notNumber _   = True