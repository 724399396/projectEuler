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
