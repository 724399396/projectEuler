import System.IO
import Data.List.Split.Internals
import Data.List
import Data.Char

zipWithIndex :: [a] -> [(Int,a)]
zipWithIndex = zip [1..]

main :: IO ()
main = do
  con <- readFile "/home/liwwli/p022_names.txt"
  putStrLn $ show $ sum  $ map (\(i,x) -> i*x) $ zipWithIndex $ map (\y -> sum $ map (\x -> (ord x) - (ord 'A') + 1) y) $ sort $ splitOn "," $ filter (/= '"') con 
