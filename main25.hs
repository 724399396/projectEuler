fibs :: [Integer]
fibs = 1:1: zipWith (+) fibs (tail fibs)

zipWithIndex :: [a] -> [(Int,a)]
zipWithIndex = zip [1..]

main :: IO ()
main = putStrLn $ show $ head $ dropWhile (\(i,x) -> x<10^999) $ zipWithIndex fibs
