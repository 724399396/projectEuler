import Data.List

a = [1,2,5,10,20,50,100]

make [] _ = []
make _ 0 = [[]]
make _ x | x < 0 = []
make (x:xs) m = (make xs m) ++ (map (\y -> x:y) (make (x:xs) (m-x)))

res total = nub $ map sort $ filter (\x -> sum x == total) $ make a total
