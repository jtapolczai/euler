module Euler where

import Data.List
import qualified Data.Map as M

-- Helper functions
-------------------------------------------------------------------------------

-- |Prime factorization.
--  Based on http://stackoverflow.com/a/1140100 and
--  https://wiki.haskell.org/Prime_numbers#Map-based
primesMPE :: [Integer]
primesMPE = 2:mkPrimes 3 M.empty prs 9   -- postponed addition of primes into map;
   where                                  -- decoupled primes loop feed
   prs = 3:mkPrimes 5 M.empty prs 9
   mkPrimes n m ps@ ~(p:t) q = case (M.null m, M.findMin m) of
      (False, (n2, skips)) | n == n2 -> mkPrimes (n+2) (addSkips n (M.deleteMin m) skips) ps q
      _ -> if n<q then    n : mkPrimes (n+2)  m                  ps q
                  else        mkPrimes (n+2) (addSkip n m (2*p)) t (head t^2)

   addSkip n m s = M.alter (Just . maybe [s] (s:)) (n+s) m
   addSkips = foldl' . addSkip


isPalindrome :: Integer -> Bool
isPalindrome x = if len `mod` 2 == 0
                 then take len2 x' == reverse (drop len2 x')
                 else take len2 x' == reverse (drop (len2+1) x')
   where
      x' = show x
      len = length $ show x
      len2 = len `div` 2

-- |The sum of all numbers less than 1000 divisible by
--  3 or 5.
problem1 :: Integer
problem1 = sum $ filter ((||) <$> divBy 3 <*> divBy 5) [1..999]
   where
      divBy x y = y `mod` x == 0

-- |The sum of all even Fibonacci numbers less than
--  4M.
problem2 :: Integer
problem2 = sum $ takeWhile lt $ filter isEven $ fibs
   where
      isEven x = x `mod` 2 == 0
      lt x = x <= 4000000
      fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

-- |The largest prime factor of a certain number.
problem3 :: Integer
problem3 = head $ filter primeFactor $ reverse $ takeWhile lt primesMPE
   where
      num = 600851475143
      lt x = x <= ceiling (sqrt $ fromIntegral num)
      primeFactor x = num `mod` x == 0

-- |The largest number that's the product of two three-digit numbers that's
--  also a palindrome.
problem4 :: Integer
problem4 = head $ reverse $ sort $ [x*y | x <- [100..999], y <- [100..999], isPalindrome (x*y)]
   where




-- |The least common multiple of [1..20], done by hand.
problem5 :: Integer
problem5 = 5 * 7 * 9 * 11 * 13 * 16 * 17 * 19
