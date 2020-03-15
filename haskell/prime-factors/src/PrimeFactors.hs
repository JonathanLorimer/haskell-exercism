module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors 0 = []
primeFactors 1 = []
primeFactors n = go 2 n
  where
    go _ 1 = []
    go x n = if n `mod` x == 0 && isPrime x
                then x : go 2 (n `div` x)
                else go (x + 1) n

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = case [x | x <- [2 .. n-1], mod n x == 0] of
              [] -> True
              _  -> False
