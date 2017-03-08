import Data.Numbers.Primes
import Numbers
import Control.Monad

numConcat x y = let xBits = num2Bits x
                    yBits = num2Bits y
                in map (bits2Num . concat . map reverse) [[xBits,yBits],[yBits,xBits]]

concatIsPrime x y = all isPrime $ numConcat x y

res =  sum $ head $ do
  a <- primes
  b <- takeWhile (<a) primes
  guard $ concatIsPrime a b
  c <- takeWhile (<b) primes
  guard $ concatIsPrime a c
  guard $ concatIsPrime b c
  d <- takeWhile (<c) primes
  guard $ concatIsPrime a d
  guard $ concatIsPrime b d
  guard $ concatIsPrime c d
  e <- takeWhile (<d) primes
  guard $ concatIsPrime a e
  guard $ concatIsPrime b e
  guard $ concatIsPrime c e
  guard $ concatIsPrime d e
  return [a,b,c,d,e]
                            
main :: IO ()
main = print res
