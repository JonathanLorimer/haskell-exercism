module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString Nothing Nothing = unlines blankBoard
boardString Nothing (Just (r, f)) = unlines $
  modNth r (modNth (f * 2) (const 'B')) blankBoard
boardString (Just (r, f)) Nothing = unlines $
  modNth r (modNth (f * 2) (const 'W')) blankBoard
boardString (Just (r, f)) (Just (r', f')) =
  unlines $ modNth r' (modNth (f' * 2) (const 'B')) board
    where board = modNth r (modNth (f * 2) (const 'W')) blankBoard


canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack w@(r,f) b@(r',f') =
  r == r'|| f == f' || b1 == w1 || b2 == w2
  where
    (b1, b2) = getDiag b
    (w1, w2) = getDiag w

getDiag :: (Int, Int) -> (Int, Int)
getDiag (r, f) = (r - i, r + i)
  where
    i = f - 1

blankBoard :: [String]
blankBoard = [ "_ _ _ _ _ _ _ _"
             , "_ _ _ _ _ _ _ _"
             , "_ _ _ _ _ _ _ _"
             , "_ _ _ _ _ _ _ _"
             , "_ _ _ _ _ _ _ _"
             , "_ _ _ _ _ _ _ _"
             , "_ _ _ _ _ _ _ _"
             , "_ _ _ _ _ _ _ _" ]

modNth :: Int -> (a -> a) -> [a] -> [a]
modNth _ _ []     = []
modNth 0 f (x:xs) = f x : xs
modNth n f (x:xs) = x : modNth (n - 1) f xs
