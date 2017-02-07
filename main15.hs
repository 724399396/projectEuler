import Data.List
import qualified Data.Set as S

type Location = (Int,Int)

boardX = 20
boardY = 20

stepBack :: Location -> [Location]
stepBack (x,y) = [(x-1,y), (x,y-1)]

legalStep :: Location -> Bool
legalStep (x,y) = x >= 0 && y >= 0

iteractStep :: Int -> [Location]
iteractStep 0 = [(boardX, boardY)]
iteractStep n = [x|y <- iteractStep (n-1), x<- stepBack y, legalStep x]

main :: IO ()
main = putStrLn $ show $ length $ iteractStep (boardX + boardY)
