import Data.List

collatz :: Int -> Int
collatz x
  | even x = quot x 2
  | otherwise = 3*x + 1

maximumByLength :: (Int,Int) -> (Int,Int) -> (Int,Int)
maximumByLength (an,as) (n,s) = if (s>as) then (n,s) else (an,as)

main :: IO ()
main = putStrLn $ show $ foldl' maximumByLength (0,0) $ zip [1..1000000] $ map (\x -> length $ takeWhile (/= 1) $ iterate collatz x) [1..1000000]
