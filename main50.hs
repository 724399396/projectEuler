import Data.Numbers.Primes
import Data.List
import Data.Maybe

accAndSum (acc,sum) a = (acc+1,sum+a)

safeHead [] = Nothing
safeHead (x:_) = Just x

sortDesc = sortBy (\(l1,_) (l2,_) ->  compare l2 l1)

res = head $ sortDesc $ map fromJust $ takeWhile isJust $ map (safeHead . sortDesc . filter (isPrime.snd).takeWhile ((<=1000000).snd).scanl accAndSum (0,0)) $ tails primes
