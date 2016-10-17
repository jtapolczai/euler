module Euler where

problem1 :: Integer
problem1 = sum $ filter ((||) <$> divBy 3 <*> divBy 5) [1..999]
   where
      divBy x y = y `mod` x == 0
