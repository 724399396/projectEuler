import Data.List

intList2Num :: [Int] -> Int
intList2Num [] = 0
intList2Num (x:xs) = x + 10 * (intList2Num xs)

main :: IO ()
main = putStrLn . show $ head $ drop (1000000 - 1) $ sort $ map intList2Num $ permutations [0..9]
