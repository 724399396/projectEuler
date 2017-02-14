import Data.List

sortedNum2Bits = sort . help
  where
    help 0 = []
    help x = (x `mod` 10): help (x `div` 10)

multipleNSameBits x n = sortedNum2Bits x == sortedNum2Bits (x * n)

res = head $ [x| x <- [1..], all (multipleNSameBits x) [2..6]]
