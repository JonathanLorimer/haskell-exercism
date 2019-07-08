module DND ( Character(..)
           , ability
           , modifier
           , character
           ) where

import Test.QuickCheck (Gen, choose, vectorOf)
import Data.List (delete)

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier con = (con - 10) `div` 2

ability :: Gen Int
ability = fmap (sum . (\xs -> delete (minimum xs) xs)) . vectorOf 4 $ choose (1, 6)

character :: Gen Character
character =  calculateHitpoints
         <$> (Character 
         <$> strength
         <*> dexterity
         <*> constitution
         <*> intelligence
         <*> wisdom
         <*> charisma
         <*> pure 0)
  where
    strength     = ability
    dexterity    = ability
    constitution = ability
    intelligence = ability
    wisdom       = ability
    charisma     = ability

calculateHitpoints :: Character -> Character
calculateHitpoints char = char { hitpoints = 10 + (modifier (constitution char)) }