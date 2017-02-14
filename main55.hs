import Data.List

maxIteration = 50

num2Bits :: Integer -> [Integer]
num2Bits 0 = []
num2Bits n = (mod n 10): (num2Bits $ div n 10)

numReverse :: Integer -> Integer
numReverse = foldl' (\acc x -> acc*10 + x) 0 . num2Bits

isPalindrome :: Integer -> Bool
isPalindrome n = let bits = num2Bits n
                 in bits == reverse bits

polindromIteration :: Integer -> Int -> Bool
polindromIteration _ 1 = False
polindromIteration n leftIters = let nextNum = n + numReverse n
                                 in
                                    if (isPalindrome nextNum)
                                    then True
                                    else polindromIteration nextNum (leftIters-1)


res = [x|x<-[1..10000], not $ polindromIteration x maxIteration]
