digitExtract :: Integer -> [Integer]
digitExtract 0 = []
digitExtract a = (rem a 10) : (digitExtract $ quot a 10)

intPowN :: [Integer] -> Integer
intPowN [] = 0
intPowN (x:xs) = (x ^ 5) + (intPowN xs)

res = [x|x <- [10000..99999], x == (intPowN $ digitExtract x)]

intList2Int :: [Integer] -> Integer
intList2Int xs = foldl (\acc x -> acc * 10 + x) 0 xs

a = [[a,b,c,d,e]|a <- [0..9], b <- [0..9], c <- [0..9], d <- [0..9], e <- [0..9]]

res2 = filter (\x -> (intList2Int x >= 10000) && (intList2Int x < 100000) && ((intList2Int x) == (intPowN x))) a 

