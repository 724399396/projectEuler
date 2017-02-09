import Data.List.Split
import Data.Maybe
import Control.Monad

triangleNum = map (\x -> div (x*(x+1)) 2) [1..]

isTriangleNum = ap elem $ flip takeWhile triangleNum . flip (<=)

numList = zip ['A'..'Z'] [1..]

word2NumSum = fmap sum . sequence . map (flip lookup numList)

main :: IO ()
main = do
  con <- readFile "p042_words.txt"
  let words = map (tail.init) $ splitOn "," con
      wordSum = map (fromJust . word2NumSum) words
  putStrLn $ show $ length $ filter isTriangleNum wordSum                 
  
