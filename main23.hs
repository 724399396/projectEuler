divisor :: Int -> [Int]
divisor n = filter (\x -> n `mod` x == 0 && n `quot` x /= 1) [1..n]

abundant = filter (\x -> x < (sum $ divisor x)) [1..28123]

sumAbundant =  [x+y|x <- abundant, y <- abundant]

main :: IO ()
main = do
  putStrLn $ show $ sum $ filter (\x -> not $ elem x sumAbundant) [1..28123]

