import Data.List

oneToNineList = drop 2 $ take 10 $ inits [1..]

digit = reverse . help
  where
    help 0 = []
    help n = (mod n 10) : help (quot n 10)

unDigit xs = foldl (\acc x -> acc*10 + x) 0 xs    

help :: Int -> [Int] -> [Int]
help n l = concat $ map digit $ map (*n) l

res = maximum $ map unDigit $ [fDigits|x <- [1..9999], l <- oneToNineList, let fDigits = help x l, sort fDigits == [1..9]]
