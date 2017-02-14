fact :: (Int -> Integer) -> Int -> Integer
fact mf 0 = 1
fact mf n = toInteger n * mf (n-1)

faster_fact n = map (fact faster_fact) [0..] !! n

combinatorics m n = faster_fact m `div` (faster_fact n) `div` (faster_fact (m-n))

res = length [num|m<-[1..100],n<-[1..m],let num = combinatorics m n, num > 1000000]
