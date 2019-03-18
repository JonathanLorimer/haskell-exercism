module Anagram (anagramsFor) where
import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (findNonAnagrams xs)

findNonAnagrams :: String -> String -> Bool
findNonAnagrams master match
    | master' == match' = False
    | sort master' == sort match' = True
    | otherwise = False
    where
        master' = map toLower master
        match' = map toLower match