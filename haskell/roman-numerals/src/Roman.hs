module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals x =
  if x > 3000 || x < 0
    then Nothing
    else Just $ go x
  where
    go :: Integer -> String
    go n
      | n == 0 = []
      | n < 4 = 'I' : go (n - 1)
      | n == 4 = 'I' : 'V' : go (n - 4)
      | n < 9 && n >= 5 = 'V' : go (n - 5)
      | n == 9 = 'I' : 'X' : go (n - 9)
      | n < 40 && n >= 10 = 'X' : go (n - 10)
      | n < 50 && n >= 40 = 'X' : 'L' : go (n - 40)
      | n < 90 && n >= 50 = 'L' : go (n - 50)
      | n < 100 && n >= 90 = 'X' : 'C' : go (n - 90)
      | n < 400 && n >= 100 = 'C' : go (n - 100)
      | n < 500 && n >= 400 = 'C' : 'D' : go (n - 400)
      | n < 900 && n >= 500 = 'D' : go (n - 500)
      | n < 1000 && n >= 900 = 'C' : 'M' : go (n - 900)
      | n >= 1000 = 'M' : go (n - 1000)
      | otherwise = error "should never reach here"

