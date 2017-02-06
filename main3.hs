import System.Environment

primes :: [Integer]
primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

intSqrt :: Integer -> Integer
intSqrt = floor . sqrt . fromIntegral

a = 600851475143

main :: IO ()
main = do
  putStrLn $ show $ filter (\x -> a `mod` x == 0)  $ takeWhile (< intSqrt a) primes
