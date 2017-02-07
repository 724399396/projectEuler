import qualified Data.Vector as V
import qualified Data.List as L

data Month = Jan | NoJan deriving (Show,Eq)

year2day n 
  | (mod n 400) == 0 = 366
  | (mod n 100) == 0 = 365
  | (mod n 4) == 0 = 366
  | otherwise = 365

buildDayIndexWithMonth :: Int -> Int -> [Int] -> Month -> V.Vector (Int,Int,Month)
buildDayIndexWithMonth y lyld range month = V.fromList $ map (\d -> (d,y,month)) $ map (+lyld) range

dayWithMonth = snd $ foldl (\(lyld,acc) y -> let yearDays = year2day y in  (lyld + yearDays, acc V.++ buildDayIndexWithMonth y lyld [1..31] Jan V.++ buildDayIndexWithMonth y lyld [32..yearDays] NoJan)) (0,V.empty) [1900..2000]

main = putStrLn $ show $ L.sum $ L.map (\x -> length x) $ L.groupBy (\(_,y1,_) (_,y2,_) -> y1 == y2) $ V.toList $ V.filter (\(i,y,m) -> m == Jan && i `mod` 7 == 0 && y > 1900) dayWithMonth
