import System.CPUTime
import Text.Printf

-- Problem 31
-- (**) Determine whether a given integer number is prime.

isPrime :: Integer -> Bool
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
    aux ((mult, prime):rest) = (prime - 1) * prime ^ (mult - 1) * aux rest


-- Problem 37
-- Efficient totient (using prime factor simplification)


-- Timing wrapper
timeIt :: IO a -> IO a
timeIt action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12) -- convert picoseconds to seconds
  printf "Execution time: %.6f sec\n" (diff :: Double)
  return result

-- Main function
main :: IO ()
main = do
  timeIt $ print (totient 100900)
  timeIt $ print (effTotient 100900)
