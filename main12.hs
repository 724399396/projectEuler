divisor :: Int -> [Int]
divisor n = filter (\x -> n `mod` x == 0) [1..n]

firstNDivisor n = head $ dropWhile (\x -> length x < n)  $ map divisor $ drop 1 $ scanl (+) 0 [1..]

main :: IO ()
main = putStrLn $ show $ firstNDivisor 500
