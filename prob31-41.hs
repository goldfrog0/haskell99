-- |

module Arithmetic where
import Distribution.System (Arch(I386))

-- Problem 31
-- (**) Determine whether a given integer number is prime.

isPrime :: Int-> Bool
isPrime 2 = True
isPrime n
  | n < 2 = False
  | even n = False
  | otherwise = null [ x | x <- [3,5..squareRootN], mod n x == 0]
  where squareRootN = floor . sqrt $ fromIntegral n

-- Problem 32
-- (**) Determine the greatest common divisor of two positive integer numbers.

myGCD :: Int -> Int -> Int
myGCD a 0 = a
myGCD 0 b = b
myGCD a b = myGCD b $ abs $ mod a b

-- Problem 33
-- (*) Determine whether two positive integer numbers are coprime.
coprime :: Int -> Int ->  Bool
coprime a b = myGCD a b == 1

-- Problem 34
-- (**) Calculate Euler's totient function phi(m).

totient :: Int -> Int
totient a = length [x | x <- [1..(a - 1)], coprime a x]

-- Problem 35
-- (**) Determine the prime factors of a given positive integer

primeFactors :: Int -> [Int]
primeFactors 1 = [1]
primeFactors n = aux n [2..n]
  where
    aux a [] = []
    aux 1 _  = []
    aux n l@(x:xs)
      | mod n x == 0 = x : aux (div n x) l
      | otherwise     = aux n xs

--Problem 36
-- (**) Determine the prime factors and their multiplicities of a given positive integer.

rlEncode :: Eq a => [a] -> [(Int, a)]
rlEncode list = map (\x -> (length x, head x) ) $ pack list

pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x:takeWhile (==x) xs): pack (dropWhile (==x) xs)

primeFactorMultiplicity :: Int -> [(Int, Int)]
primeFactorMultiplicity = rlEncode . primeFactors

-- Problem 37
-- Efficient totient (using prime factor simplification)

effTotient :: Int -> Int
effTotient n = aux $ primeFactorMultiplicity n
  where
    aux [] = 1
    aux ((mult, num):rest) = (num - 1)^mult * aux rest

-- Problem 38
-- (*) A list of prime numbers in a given range. Solutions

primesInRange :: Int -> Int -> [Int]
primesInRange start end = [x | x <- [start..end], isPrime x]

-- Problem 40
-- (**) Goldbach's conjecture. Solutions
-- for even numbers greater than 2, find the two primes,
-- that when added, equal that number.

goldbach :: Int -> (Int, Int)
goldbach n
  | even n = aux (primesInRange 1 n)  n
  | n < 0  = (0, 0)
  | otherwise = (0, 0)
  where
    aux :: [Int] -> Int -> (Int, Int)
    aux (a:b:xs) target
      | a + b == target = (a , b)
      | otherwise       = aux (a:xs) target

data ReducedSqrt = Irrational Int Int | Perfect Int

instance Show ReducedSqrt where
    show :: ReducedSqrt -> String
    show (Irrational a b) = show a ++ " sqrt(" ++ show b ++ ")"
    show (Perfect a)      = show a

rlToNum :: [(Int, Int)] -> Int
rlToNum [] = 1
rlToNum ((_, prime):xs) = prime * rlToNum xs

reduceInnerSqrt :: Int -> Int
reduceInnerSqrt a = go (primeFactorMultiplicity a)
  where
    go ls = rlToNum $ filter (\(mult, prime) -> odd mult) ls

reduceOuterSqrt :: Int -> Int
reduceOuterSqrt a = rlToNumSquares $ clean (primeFactorMultiplicity a)
  where
    clean = filter (\(mult, prime) -> mult > 1)
    rlToNumSquares [] = 1
    rlToNumSquares ((mult, prime):rest)
      | even mult = prime ^ div mult 2 * rlToNumSquares rest
      | otherwise = prime ^ div (mult - 1) 2 * rlToNumSquares rest

intToReducedSqrt :: Int -> ReducedSqrt
intToReducedSqrt x
  | reduceInnerSqrt x == 1 = Perfect $ reduceOuterSqrt x
  | otherwise              = Irrational (reduceOuterSqrt x) (reduceInnerSqrt x)

rSqrtMult :: ReducedSqrt -> ReducedSqrt -> ReducedSqrt
rSqrtMult (Perfect a) (Perfect b) = Perfect (a*b)
rSqrtMult (Perfect a) (Irrational outer inner) = Irrational (a*outer) inner
rSqrtMult (Irrational outer inner) (Perfect b) = Irrational (b*outer) inner
rSqrtMult (Irrational a b) (Irrational x y)    = intToReducedSqrt (a*a*x*x*b*y)

isPerfectSquare n = case (intToReducedSqrt n) of
                      (Perfect n) -> True
                      _           -> False

perfectSquares :: [Int]
perfectSquares = [x | x <- [1..], isPerfectSquare x]
