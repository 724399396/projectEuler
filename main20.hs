factorial 1 = 1
factorial n = n * factorial (n - 1)

num2DigitList :: Integer -> [Integer]
num2DigitList 0 = []
num2DigitList n = (mod n 10) : (num2DigitList $ quot n 10)

main :: IO ()
main = putStrLn $ show $ sum $ num2DigitList $ factorial 100
