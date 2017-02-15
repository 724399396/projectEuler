module Numbers where

num2Bits :: Integer -> [Integer]
num2Bits 0 = []
num2Bits n = (mod n 10): (num2Bits $ div n 10)
