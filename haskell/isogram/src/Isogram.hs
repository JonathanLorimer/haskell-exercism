{-# LANGUAGE OverloadedStrings #-}

module Isogram (isIsogram) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Set (Set, empty, insert, notMember)

isIsogram :: Text -> Bool
isIsogram = fst 
          . T.foldl' checkDuplicate (True, empty)
          . T.toUpper
          . T.filter (\x -> x /= '-' && x /= ' ')

checkDuplicate :: (Bool, Set Char) -> Char -> (Bool, Set Char)
checkDuplicate (False, _) _ = (False, empty)
checkDuplicate (_, s) c = if notMember c s
                          then (True, insert c s)
                          else (False, empty)

