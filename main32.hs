import Data.List
import qualified Data.Set as Set

digitExtract :: Integer -> [Integer]
digitExtract 0 = []
digitExtract a = (rem a 10) : (digitExtract $ quot a 10)

res = Set.toList $ Set.fromList $ map snd $ filter (\(x,_) -> sort x == [1..9]) [(digitExtract a ++ digitExtract b ++ digitExtract (a * b), a*b) | a <- [0..9999], b <- [a..9999]]
