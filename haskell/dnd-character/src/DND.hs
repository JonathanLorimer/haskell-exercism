module DND ( Character(..)
           , ability
           , modifier
          --  , character
           ) where

import Test.QuickCheck (Gen, choose)

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
ability = choose (1, 6)

character :: Gen Character
character = Character strength dexterity constitution intelligence wisdom charisma hitpoints
  where
    strength     = ability
    dexterity    = ability
    constitution = ability
    intelligence = ability
    wisdom       = ability
    charisma     = ability
    hitpoints    = modifier constitution
