module Primes where

import qualified System.Random as R
import qualified Data.Maybe


modPow :: (Integral n) => n -> n -> n -> n
-- calculates a^r mod m
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

millerRabin :: (Integral n, R.Random n, R.RandomGen g) => g -> n -> n -> Bool
millerRabin g s p = all (\(_, z) -> z == 1 || z == p - 1 || cond (1 :: Integer) z) (zip [1 .. s] zs)
  where
    decompose q = head $ dropWhile (even . snd) $ iterate (\(i, x) -> (i + 1, x `div` 2)) (0, q)
    (u, r) = decompose (p - 1)
    as = R.randomRs (2, p - 2) g
    zs = map (\a -> modPow a r p) as
    cond j z
      | u == 0 = False
      | j == u = z == p - 1
      | otherwise =
          let z' = (z * z) `mod` p
           in (z' /= 1) && cond (j + 1) z'