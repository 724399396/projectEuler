import Data.List
import qualified Data.Set as S

digitExtract :: Integer -> [Integer]
digitExtract 0 = []
digitExtract a = (rem a 10) : (digitExtract $ quot a 10)

intPowN :: Integer -> [Integer] -> Integer
intPowN _ [] = 0
intPowN n (x:xs) = (x ^ n) + (intPowN n xs)

res = sum $ map fst $ filter (\(product, x) -> intPowN 5 x == product) $ map (\x -> (x, digitExtract x)) [2..999999]

