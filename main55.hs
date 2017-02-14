import Data.List

maxIteration = 15

num2Bits 0 = []
num2Bits n = (mod n 10): (num2Bits $ div n 10)

numReverse :: Int -> Int
numReverse = foldl' (\acc x -> acc*10 + x) 0 . num2Bits

isPalindrome n = let bits = num2Bits n
                 in bits == reverse bits

polindromIteration _ 0 = False
polindromIteration n leftIters = let nextNum = n + numReverse n
                                 in
                                    if (isPalindrome nextNum)
                                    then True
                                    else polindromIteration nextNum (leftIters-1)


res = [x|x<-[1..10000], not $ polindromIteration x maxIteration]
