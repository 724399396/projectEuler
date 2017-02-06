type Location = (Int,Int)

boardX = 20
boardY = 20

stepBack :: Location -> [Location]
stepBack (x,y) = [(x-1,y), (x,y-1)]

legalStep :: Location -> Bool
legalStep (x,y) = x <= boardX && y <= boardY && x >= 0 && y >= 0

iteractStep :: Int -> [[Location]]
iteractStep 0 = [[(boardX, boardY)]]
iteractStep n = let left = iteractStep (n-1)
                in
                  [x:xs|xs <- left, x<- stepBack (head xs), legalStep x]

main :: IO ()
main = putStrLn $ show $ length $ iteractStep (boardX * boardY)
