divisor :: Int -> [Int]
divisor n = filter (\x -> n `mod` x == 0 && quot n x /= 1) [1..n]

sumDivosor :: Int -> Int
sumDivosor = sum . divisor

amicable :: Int
amicable = let src = zip [1..10000] $ map sumDivosor [1..10000]
           in
             sum [a| (a,sa)<-src, (b,sb)<-src, a /= b && a == sb && b == sa]

main :: IO ()
main = putStrLn $ show $ amicable
