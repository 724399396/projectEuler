import Control.Monad
import Data.Numbers.Primes

twiceASquare = map (join(+).join(*)) [0..]

match n = [(x,y,n)|x <- (takeWhile (<=n) primes),y <- (takeWhile (<=n) twiceASquare), x+y == n]

res = head [x| x<- [3,5..], null $ match x]
