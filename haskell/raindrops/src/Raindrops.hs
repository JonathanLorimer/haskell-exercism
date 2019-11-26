module Raindrops (convert) where

import Data.Maybe
import Data.Foldable
import Control.Applicative

convert :: Int -> String
convert n = defAsum (show n) . catMaybes $ do
  f <- fmap (\(fs,sn) -> mult sn fs) ws
  pure $ f n

defAsum :: (Eq (t (f a)), Monoid (t (f a)), Foldable t, Alternative f) => f a -> t (f a) -> f a
defAsum d tfa
  | tfa == mempty = d
  | otherwise     = asum tfa

ws :: [(Int, String)]
ws = [(3, "Pling"), (5, "Plang"), (7, "Plong")]

mult :: String -> Int -> Int -> Maybe String
mult s i m
  | m `mod` i == 0 = Just s
  | otherwise      = Nothing


