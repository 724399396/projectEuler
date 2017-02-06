twoPowN :: Integer -> Integer
twoPowN n = floor $ 2 ** (fromIntegral n )

num2DigitList :: Integer -> [Integer]
num2DigitList 0 = []
num2DigitList n = (mod n 10) : (num2DigitList $ quot n 10)

powerSum n = sum $ num2DigitList n

main :: IO ()
main = putStrLn $ show $ powerSum (twoPowN 1000)
