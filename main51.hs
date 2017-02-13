import Data.List

totalBits :: Int
totalBits = 5

arrangements :: [[([Int],[Int])]]
arrangements = map lengthNArrange [1..(totalBits-1)]
  where
    lengthNArrange :: Int -> [([Int],[Int])]
    lengthNArrange n = let
      bases = filter ((==(totalBits-n)).length) $ subsequences [0..(totalBits-1)]
      in
        map (\base -> (base, [0..(totalBits-1)] \\ base)) bases



bitReplace :: [Int] -> [Int] -> [Int] -> [Int]
bitReplace templateNumList keepLocs replaceLocs =
  let base = digitList2Num templateNumList keepLocs
      replaceLength = length replaceLocs
      digitList2Num ns xs = sum $ map (\x -> (ns !! x) * (10 ^ (totalBits-1-x)))  xs -- [0,1,2,3,4]
  in
    [n|x <- [0..9], let n = base + digitList2Num (replicate totalBits x) replaceLocs, n >= 10 ^ (totalBits-1)]
      
