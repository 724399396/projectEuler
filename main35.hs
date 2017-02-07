import Data.List
import Data.Numbers.Primes

digitExtract 0 = []
digitExtract a = (digitExtract $ quot a 10) ++ [rem a 10]

intList2Num = foldl (\acc x -> acc * 10 + x) 0

rotationList :: [a] -> [[a]]
rotationList xs = map help [1..(xsLength - 1)]
  where xsLength = length xs
        help n = drop n xs ++ take n xs

rotations :: Int -> [Int]
rotations n = map intList2Num $ rotationList $ digitExtract n

res = filter (all isPrime . rotations) $ takeWhile (<1000000) primes


slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-2) + slow_fib (n-1)

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)
