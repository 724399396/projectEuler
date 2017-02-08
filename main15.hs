import qualified Data.Vector as V

type Location = (Int,Int)

boardSize = 20

stepBack :: Location -> V.Vector Location
stepBack (x,y) = V.fromList [(x-1,y), (x,y-1)]

legalStep :: Location -> Bool
legalStep (x,y) = x >= 0 && y >= 0

iteractStep :: Int -> V.Vector (V.Vector Location)
iteractStep 0 = V.fromList [V.fromList [(boardSize, boardSize)]]
iteractStep n = do
  y <- iteractStep (n-1)
  x <- stepBack (V.head y)
  if (legalStep x)
  then return $ V.cons x y
  else fail "" 

res = length $ iteractStep (2*boardSize)

f :: (Int -> Integer) -> Int -> Integer
f mf 0 = 1
f mf n = toInteger n * (mf (n-1))

f_list :: [Integer]
f_list = map (f faster_f) [0..]

faster_f :: Int -> Integer
faster_f n = f_list !! n

res2 = faster_f (2*boardSize) `div` (faster_f boardSize * faster_f boardSize)
