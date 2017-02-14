import Data.List
import Data.Numbers.Primes

expectPrimeNum = 8

num2Bits = reverse . help
  where
    help 0 = []
    help x = (x `mod` 10): help (x `div` 10)


arrangements :: Int -> [([Int],[Int])]
arrangements totalBits = concat $ map lengthNArrange [1..(totalBits-1)]
  where
    lengthNArrange :: Int -> [([Int],[Int])]
    lengthNArrange n = let
      bases = filter ((==(totalBits-n)).length) $ subsequences [0..(totalBits-1)]
      in
        map (\base -> (base, [0..(totalBits-1)] \\ base)) bases



bitReplace :: [Int] -> [Int] -> [Int] -> Int -> [Int]
bitReplace templateNumList keepLocs replaceLocs totalBits =
  let base = digitList2Num templateNumList keepLocs
      replaceLength = length replaceLocs
      digitList2Num ns xs = sum $ map (\x -> (ns !! x) * (10 ^ (totalBits-1-x)))  xs -- [0,1,2,3,4]
  in
    [n|x <- [0..9], let n = base + digitList2Num (replicate totalBits x) replaceLocs, n >= 10 ^ (totalBits-1)]
      
res = head [filter isPrime nums|x<-[10..], totalBits <- [2..(length$num2Bits x)], (keepLocs,replaceLocs) <- arrangements totalBits, let nums = bitReplace (num2Bits x) keepLocs replaceLocs totalBits, (==expectPrimeNum) $ length $ filter isPrime nums]
