module Phone (number) where

import Data.Char (isDigit, digitToInt)

number :: String -> Maybe String
number xs
    |      length cleaned == 11
        && cleaned !! 0 == '1'
        && isValid (tail cleaned) = Just (tail cleaned)
    |      length cleaned == 10 
        && isValid cleaned = Just cleaned
    |      otherwise = Nothing
        where
            cleaned = filter isDigit xs

isValid :: String -> Bool
isValid xs =  2 <= digitToInt (xs !! 0) 
           && 2 <= digitToInt (xs !! 3)

