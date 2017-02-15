import Data.Numbers.Primes

pair4Last (_,_,_,x) = x

n2Length n = (n `div` 4) * 2 + 1

length2N l = (l-1)*2

spiralDiagonals n = pair4Last $ foldl (\(n,gap,last,res) _ -> if (n==4)
                               then (1,gap+2,last+gap,(last+gap):res)
                               else (n+1,gap,last+gap,(last+gap):res)) (1,2,1,[1]) [1..n]



primePercent xs = (fromIntegral $ length $ filter isPrime xs) / (fromIntegral $ length xs)

spiralDiagonalPercent = map (\n -> (n,primePercent$spiralDiagonals$length2N n)) [3,5..]

res = head $ dropWhile (\(_,percent) -> percent > 0.1) spiralDiagonalPercent

