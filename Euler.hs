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

-- |Returns the prime factors of a number.
factorize :: Integer -> [Integer]
factorize num = factors
   where
      factors = filter primeFactor $ takeWhile lt primesMPE
      lt x = x <= num
      primeFactor x = num `mod` x == 0

-------------------------------------------------------------------------------

-- |The sum of all numbers less than 1000 divisible by
--  3 or 5.
problem1 :: Integer
problem1 = sum $ filter ((||) <$> divBy 3 <*> divBy 5) [1..999]
   where
      divBy x y = y `mod` x == 0

-- |The sum of all even Fibonacci numbers less than 4M.
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

-- |The difference between (1²+2²+...+100²) and (1+2+...+100)².
problem6 :: Integer
problem6 = abs $ sumOfSquares [1..100] - squareOfSum [1..100]
   where
      sumOfSquares = sum . map (\x -> x * x)
      squareOfSum = (\x -> x * x) . sum

-- |The 10 001st prime number
problem7 :: Integer
problem7 = primesMPE !! 10000

-- |The thirteen adjacent digits in a number that have the greatest
--  product.
problem8 :: Integer
problem8 = head $ reverse $ sort $ map productOf $ windows 13 num
   where
      productOf :: String -> Integer
      productOf = product . map (read . (:[]))

      windows n xs = map (\i -> take n $ drop i xs) [1..length xs - n]

      num = "73167176531330624919225119674426574742355349194934\
             \96983520312774506326239578318016984801869478851843\
             \85861560789112949495459501737958331952853208805511\
             \12540698747158523863050715693290963295227443043557\
             \66896648950445244523161731856403098711121722383113\
             \62229893423380308135336276614282806444486645238749\
             \30358907296290491560440772390713810515859307960866\
             \70172427121883998797908792274921901699720888093776\
             \65727333001053367881220235421809751254540594752243\
             \52584907711670556013604839586446706324415722155397\
             \53697817977846174064955149290862569321978468622482\
             \83972241375657056057490261407972968652414535100474\
             \82166370484403199890008895243450658541227588666881\
             \16427171479924442928230863465674813919123162824586\
             \17866458359124566529476545682848912883142607690042\
             \24219022671055626321111109370544217506941658960408\
             \07198403850962455444362981230987879927244284909188\
             \84580156166097919133875499200524063689912560717606\
             \05886116467109405077541002256983155200055935729725\
             \71636269561882670428252483600823257530420752963450"

-- |Finds the product of a,b,c in N such that a²+b²=c² and
--  a+b+c=1000.
problem9 :: Integer
problem9 = head
           $ [a*b*c | a <- [1..1000],
                      b <- [1..1000],
                      isIntegerSquare a b,
                      a+b+ intsqrt a b == 1000,
                      let c = 1000 - a - b]
   where
      isIntegerSquare a b = fromIntegral (round (sq a b)) == sq a b
      sq :: Integer -> Integer -> Float
      sq a b = sqrt $ fromIntegral $ (a*a) + (b*b)
      intsqrt a b = round $ sq a b

-- |The sum of all primes below 2M.
problem10 :: Integer
problem10 = sum $ takeWhile (<2000000) primesMPE
