import Data.List

cycleFind :: Int -> Int
cycleFind x = (length $ help 1 x []) - 1
 where help a b meets
         | elem a meets = []
         | otherwise = let m = mod a b
                       in
                         m : help (m * 10) b (a:meets)

res = head $ sortBy (\(_,l1) (_,l2) -> compare l2 l1) $ zip [1..] $ map cycleFind [1..1000]                         
