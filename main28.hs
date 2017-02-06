spiralDiagonals :: Int -> Int -> [Int] -> [Int]
spiralDiagonals step times left = let next = drop step left
                                  in  (head next) : (if (times > 0) then spiralDiagonals step (times-1) (tail next)
                                                       else spiralDiagonals (step+2) 3 (tail next))

sumDiagonals :: Int -> Int
sumDiagonals a = let list = 1 : spiralDiagonals 1 3 [2..]
                     num = (quot a 2) * 4 + 1
                 in sum (take num list)
