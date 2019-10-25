module Proverb(recite) where


finalS :: String
finalS = "And all for the want of a "

recite :: [String] -> String
recite [] = ""
recite s = (++ unique) . unlines . zipWith go s $ drop 1 s
  where
    go a b =
      concat [ "For want of a "
             , a
             , " the "
             , b
             , " was lost."

             ]
    unique :: String
    unique =
      concat [ "And all for the want of a "
             , head s
             , "."
             ]


