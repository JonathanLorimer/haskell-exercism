{-# LANGUAGE OverloadedStrings #-}
module Acronym (abbreviate) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Char (toUpper, isUpper)

abbreviate :: Text -> Text
abbreviate = T.concat 
    . map (takeSignificantCaps . upperFirst)
    . filter (\x -> x /= "")
    . T.split (\x -> (x ==' ') || (x =='-') )

upperFirst :: Text -> Text
upperFirst t = T.cons (toUpper $ T.head t) (T.tail t)

takeSignificantCaps :: Text -> Text
takeSignificantCaps word
    | T.all isUpper word = (T.singleton . T.head) word
    | otherwise = T.filter isUpper word
