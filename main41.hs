import Control.Monad
import Data.List
import Data.Numbers.Primes

--isPrime = ap (all.((/=0).).mod) $ flip takeWhile primes . (.join(*)) . flip (<=)
--primes = 2 : filter isPrime [3,5..]

match n = sort bits == [1..(length bits)]
  where
    help 0 = []
    help n = (mod n 10):help (quot n 10)
    bits = help n

res = last $ filter match $ takeWhile (<999999999) primes
