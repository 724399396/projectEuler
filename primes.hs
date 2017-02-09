import Control.Monad

isPrime = ap (all.((0/=).).mod) $ flip takeWhile primes.(.join(*)).flip(<=)
primes = 2: filter isPrime[3,5..]

primes2 :: [Integer]
primes2 = sieve [2..]
  where sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]
