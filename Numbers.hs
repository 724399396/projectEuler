module Numbers where

import Data.List

num2Bits :: Integer -> [Integer]
num2Bits 0 = []
num2Bits n = (mod n 10): (num2Bits $ div n 10)

bits2Num :: [Integer] -> Integer
bits2Num = foldl' (\acc x -> acc * 10 + x) 0
