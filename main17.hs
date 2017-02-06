num2DigitList :: Integer -> [Integer]
num2DigitList 0 = []
num2DigitList n = (mod n 10) : (num2DigitList $ quot n 10)

num2Description :: Integer -> [Char]
num2Description n = case lookup n [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five"), (6, "six"), (7, "seven"), (8, "eight"), (9, "nine")] of
  Nothing -> ""
  Just(x) -> x
