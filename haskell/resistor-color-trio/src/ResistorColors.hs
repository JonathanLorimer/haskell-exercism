module ResistorColors (Color(..), Resistor(..), label, ohms) where

import           Data.String
import           Data.Text   (Text)
import qualified Data.Text   as T

data Color = Black
           | Brown
           | Red
           | Orange
           | Yellow
           | Green
           | Blue
           | Violet
           | Grey
           | White
            deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> Text
label r
  | ohms r >= 1000000000 = printLabel (ohms r) "giga" 1000000000
  | ohms r >= 1000000 = printLabel (ohms r) "mega" 1000000
  | ohms r >= 1000 = printLabel (ohms r) "kilo" 1000
  | otherwise = printLabel (ohms r) "" 1
      where
        printLabel r p d = fromString
                       . (++ (" " ++ p ++ "ohms"))
                       . show $ r `div` d

ohms :: Resistor -> Int
ohms (Resistor (d1, d2, z)) =
  let tens = fromEnum d1 * 10
      ones = fromEnum d2
      zero = fromEnum z
  in (tens + ones) * (10 ^ zero)
