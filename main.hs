import System.Environment

primes :: [Integer]
primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

primesSum :: Integer -> Integer
primesSum x = sum $ takeWhile (< x) primes

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show $ primesSum 2000000
