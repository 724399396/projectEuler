
divideWithoutLeft :: Int -> Int
divideWithoutLeft x = head $ filter help [1..]
  where list = [1..x]
        help x = all (\y -> x `mod` y == 0) list

main :: IO ()
main = putStrLn $ show $ divideWithoutLeft 20
