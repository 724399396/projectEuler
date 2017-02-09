import Data.Numbers.Primes
import Data.List

digit = reverse . help
  where
    help 0 = []
    help n = (mod n 10) : help (quot n 10)

unDigit = foldl (\acc x -> acc*10 + x) 0    

subNum n xs = 100*(xs !! (n+1)) + 10 * (xs !! (n+2)) + (xs !! (n+3))

divisible m n = mod m n == 0

matchN xs n = divisible (subNum n xs) (primes !! n)

exceptN n = filter (/=n) [0..9]

res = sum [unDigit digits|a <- [0..9], b <- ([0..9] \\ [a]), c<-([0..9] \\ [a,b]), d<-([0..9] \\ [a,b,c]), e<-([0..9] \\ [a,b,c,d]), f<-([0..9] \\ [a,b,c,d,e]), g<-([0..9] \\ [a,b,c,d,e,f]), h<-([0..9] \\ [a,b,c,d,e,f,g]), i<-([0..9] \\ [a,b,c,d,e,f,g,h]), j<-([0..9] \\ [a,b,c,d,e,f,g,h,i]), let digits = [a,b,c,d,e,f,g,h,i,j], all (matchN digits) [0..6]]
