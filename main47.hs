import Data.Numbers.Primes
import Data.List
import Control.Monad

myPrimeFactors x = factor x primes
  where
    factor x (r:rs)
      | r > x = []
      | x `mod` r == 0 = r:factor x rs
      | otherwise = factor x rs

match ((a,4):(b,4):(c,4):(d,4):xs)
  | a+1==b && b+1==c && c+1==d  = [a,b,c,d]
  | otherwise = match ((b,4):(c,4):(d,4):xs)
match (a:b:c:d:xs) = match (b:c:d:xs)
match _ = []

res = match $ map (ap (,) $ length.nub.primeFactors) [2*3*5*7..]
