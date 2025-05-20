module Primes where

import GHC.Num (integerLog2)
import qualified System.Random as R
import Data.Ratio


-- | calculates a^r mod m
modPow :: (Integral n) => n -> n -> n -> n
modPow a r m
  | r < 0 = error "negative exponent is not supported for now"
  | otherwise = modpow_impl 1 a r
  where
    modpow_impl y _ 0 = y
    modpow_impl y z e =
      let y' = if odd e then (y * z) `mod` m else y
          z' = (z * z) `mod` m
          e' = e `div` 2
       in modpow_impl y' z' e'

-- | given generator g and security parameter s, performs Miller Rabin primality test on Integral p
--   if millerRabin returns False, then it's certain that number p is not prime
--   otherwise if millerRabin returns True, then p is likely a prime number.
millerRabin :: (Integral n, R.Random n, R.RandomGen g) => g -> n -> n -> Bool
millerRabin g s p = 
    all 
      (\(_, z) -> z == 1 || z == p - 1 || cond (1 :: Integer) z) 
      (zip [1 .. s] zs)
    where
      decompose q = 
        head $ 
        dropWhile (even . snd) $ 
        iterate (\(i, x) -> (i + 1, x `div` 2)) (0, q)
      (u, r) = decompose (p - 1)
      as = R.randomRs (2, p - 2) g
      zs = map (\a -> modPow a r p) as
      cond j z
        | u == 0 = False
        | j == u = z == p - 1
        | otherwise =
            let
              z' = (z * z) `mod` p
            in
              (z' /= 1) && cond (j + 1) z'



-- | Generates random kbits prime number that is a prime number from interval [2^(k-1), 2^k], (or [2^(k-1) + 1, 2^k - 1] to be more precise)
--   Returns the propagated generator together with the prime number.
--   The result is wrapped inside a Maybe
randomkbitsPrime :: (Integral n, R.Random n, R.RandomGen g) => g -> n -> Maybe (Integer, g)
randomkbitsPrime g k =
  case 
    dropWhile 
      (not . millerRabin g 30 . fst . snd)
      (zip [(1 :: Integer) .. n_of_samples] rs)
  of
    [] -> Nothing
    ((_, (p, g')) : _) -> Just (p, g')
  where
    ln n = 
      let 
        ln2 = 0.69314718056 :: Double 
      in
        (+ 1) $ floor $ fromIntegral (integerLog2 n) * ln2
    (a, b) = (2 ^ (k - 1), 2 ^ k)
    rs = iterate (\(_, g') -> R.randomR (a, b) g') (R.randomR (a, b) g)
    n_of_samples =
      let 
        toa = a `div` ln a
        tob = b `div` ln b
        n_of_primes = tob - toa
        is_prime_proba = fromRational (n_of_primes % (b - a)) :: Double
        err = 0.00001 :: Double
      in 
        (+ 1) $ floor $ logBase (1 - is_prime_proba) err