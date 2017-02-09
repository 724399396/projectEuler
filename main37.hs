import Data.List
import Data.Numbers.Primes

digit = reverse . help
  where
    help 0 = []
    help n = (mod n 10) : help (quot n 10)

unDigit xs = foldl (\acc x -> acc*10 + x) 0 xs

truncateN = init . drop 1 . tails
truncateLeft = map unDigit . truncateN . digit
truncateRight = map (unDigit . reverse) . truncateN . reverse . digit
truncateBoth x = (truncateLeft x) ++ (truncateRight x)

res = sum $ take 11 $ dropWhile (\x -> length (digit x) == 1) $ filter (\x -> all isPrime $ truncateBoth x) primes




