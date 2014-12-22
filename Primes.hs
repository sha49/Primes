--------------------------------------------------------------------------------
-- author: Jaume Pernas
-- Unlicense (see http://unlicense.org/)
--------------------------------------------------------------------------------
module Primes
( 
  isPrime
, nextPrime
, randomPrime
) where


import System.Random
--------------------------------------------------------------------------------
-- generates a t-bounded random list of n elements
randomList :: (RandomGen g, Random a) => g -> (a,a) -> Int -> [a]
randomList s t n = take n $ randomRs t s
--------------------------------------------------------------------------------
-- binary exponenciation algorithm :: a^e mod n
power :: (Integral a) => a -> a -> a -> a 
power a 1 n = a
power a e n = (a^(e `mod` 2) * power (a*a `mod` n) (e `div` 2) n) `mod` n 
--------------------------------------------------------------------------------
-- Given x returns (y,z) where x = 2^y * z and z odd
twoPowersFact :: (Integral a) => a -> (a,a)
twoPowersFact 0 = (0,0)
twoPowersFact m = let go (s,n)
                         | even n    = go (s+1,n `div` 2)
                         | otherwise = (s,n)
                  in go (0,abs m)
--------------------------------------------------------------------------------
-- PseudoPrimality Test :: Miller Rabin
-- millerRabin possiblePrime testBase
millerRabin :: (Integral a) => a -> a ->  Bool
millerRabin n a = let fact = twoPowersFact (n-1);
                      base = power a (snd fact) n;
                      test b s r n
                       | r == s = b == n-1 
                       | b == 1 && r /= 0 = False
                       | otherwise = b == n-1 || test (power b 2 n) s (r+1) n 
                  in base == 1 || test base (fst fact) 0 n
--------------------------------------------------------------------------------
-- isPrimePure possiblePrime [list of testBases]
isPrimePure :: (Eq a, Integral a) => a -> [a] -> Bool 
isPrimePure 1 _  = False
isPrimePure 2 _  = True
isPrimePure _ [] = False -- should not be true or false 
isPrimePure can wit = let primes = [3,5,7,11,13,17,19,23,29,31] in 
                   all (\ p -> p >= can || (can `mod` p) /= 0) primes
                && foldr ((&&) . millerRabin can) True wit
--------------------------------------------------------------------------------
-- the longer is the randomList, the higher is the accuracy of the test
isPrime :: (RandomGen g, Integral a, Random a) => g -> a -> Bool
isPrime seed can = isPrimePure can $ randomList seed (2,can-2) 20
--------------------------------------------------------------------------------
-- the next prime after num
nextPrime :: (RandomGen g, Integral a, Random a) => g -> a -> a
nextPrime seed num
    | even num         = nextPrime (snd $ next seed) (num+1)
    | isPrime seed num = num
    | otherwise        = nextPrime (snd $ next seed) (num+2)
--------------------------------------------------------------------------------
nBitsRanNum :: (RandomGen g, Integral a, Random a) => g -> a -> (a, g)
nBitsRanNum seed 0     = (0, snd $ next seed)
nBitsRanNum seed 1     = randomR (0,1) seed
nBitsRanNum seed nbits = randomR (2^(nbits-1),2^nbits-1) seed
--------------------------------------------------------------------------------
-- random prime of nbits
randomPrime :: (RandomGen g, Integral a, Random a) => g -> a -> a
randomPrime seed nbits = nextPrime seed (fst $ nBitsRanNum seed nbits) 
--------------------------------------------------------------------------------
