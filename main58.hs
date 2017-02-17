--import Data.Numbers.Primes
import Control.Monad

isPrime = ap (all.((0/=).).mod) $ flip takeWhile primes.(.join(*)).flip(<=)
primes = 2: filter isPrime[3,5..]


type Rational = (Int,Int)

spiralDiagonals mf 0 = [1]
spiralDiagonals mf 1 = [9,7,5,3,1]
spiralDiagonals mf n = let xs@(x1:x2:_) = mf (n-1)
                           diff = (x1-x2)+2
                       in
                         (map ((+x1).(*diff)) [4,3..1]) ++ xs

spiralDiagonalsList = map (spiralDiagonals faster_spiralDiagonals) [0..]                    
faster_spiralDiagonals n = spiralDiagonalsList !! n

primePercent xs = (fromIntegral $ length $ filter isPrime xs) / (fromIntegral $ length xs)

level2SideLength = (+1).(*2)

spiralDiagonalPercent mf 0 = (0,1)
spiralDiagonalPercent mf n = let (prevH, prevT) = mf (n-1)
                                 newFourNums = take 4 $ faster_spiralDiagonals n
                             in
                               (prevH + (length $ filter isPrime newFourNums) , (prevT + 4))

spiralDiagonalPercentList = map (spiralDiagonalPercent faster_spiralDiagonalPercent) [0..]                              

faster_spiralDiagonalPercent n = spiralDiagonalPercentList !! n
                               

main = putStrLn $ show $ head $ dropWhile (\(_,(h,t)) -> (fromIntegral h/ fromIntegral t) > 0.1) $ (map (\n -> (level2SideLength n, faster_spiralDiagonalPercent n)) [1..])

