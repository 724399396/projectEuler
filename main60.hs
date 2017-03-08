import Data.Numbers.Primes
import Numbers

numConcat x y = let xBits = num2Bits x
                    yBits = num2Bits y
                in bits2Num $ ((reverse xBits) ++ (reverse yBits))

res =  head [(a,b,c,d,e)| a <- primes, b <- takeWhile (<a) primes, c <- takeWhile (<b) primes, d <- takeWhile (<c) primes, e <- takeWhile (<d) primes, all isPrime $ [numConcat x y|x <- [a,b,c,d,e], y <- [a,b,c,d,e], x /= y]]
                            
main :: IO ()
main = print res
