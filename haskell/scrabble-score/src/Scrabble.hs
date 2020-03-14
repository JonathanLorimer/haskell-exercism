module Scrabble (scoreLetter, scoreWord) where

import Data.Char (isAlphaNum)

scoreLetter :: Char -> Integer
scoreLetter letter
  | letter `elem` "aeioulnrstAEIOULNRST" = 1
  | letter `elem` "dgDG" = 2
  | letter `elem` "bcmpBCMP" = 3
  | letter `elem` "fhvwyFHVWY" = 4
  | letter `elem` "kK" = 5
  | letter `elem` "jxJX" = 8
  | letter `elem` "qzQZ" = 10
  | otherwise = 0


scoreWord :: String -> Integer
scoreWord = sum . fmap scoreLetter

