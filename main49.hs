import Data.Numbers.Primes
import Data.List

ps = takeWhile (<10000) $ dropWhile (<1000) primes

isPermutation a b c = (sortDigit a) == (sortDigit b) && (sortDigit b)  == (sortDigit c)
  where
    digit 0 = []
    digit n = (mod n 10):(digit $ div n 10)
    sortDigit = sort . digit

res = [(x,y,z)| x <- ps, y <- (dropWhile (<= x) ps), z <- (dropWhile (<= y) ps), y-x == z-y, isPermutation x y z]

