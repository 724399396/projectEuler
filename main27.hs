import Data.List

quadratics r = [(a, b, help a b)|a <- [(negate r)..r], b <- [(negate r)..r]]
  where
    help a b n = n * n +  a * n + b

primes :: [Integer]
primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

filterPrimes f = takeWhile (\x -> elem x $ takeWhile (<= x) primes) $ map f [0..]

main :: IO()
main = putStrLn $ show $ last $ sortBy (\(_,_,x1) (_,_,x2) -> compare x1 x2)  $ filter (\(a,b,y) -> y> 0) $ map (\(a,b,y) -> (a,b,length $ filterPrimes y)) $ quadratics 999



