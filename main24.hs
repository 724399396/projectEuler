import Data.List

numLimit = 10

productList = foldl (\acc x -> acc * 10 + x) 0

permutationNums = map productList $ permutations [1..10]

main :: IO ()
main = putStrLn . show $ head $ drop 1000000 $ sort $ permutationNums
