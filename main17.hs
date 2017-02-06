numDescriptionMap = [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five"), (6, "six"), (7, "seven"), (8, "eight"), (9, "nine"), (10, "ten"), (11, "eleven"), (12, "twelve"), (13, "thirteen"), (14, "fourteen"), (15, "fifteen"), (16, "sixteen"), (17, "seventeen"), (18, "eighteen"), (19, "nineteen")]

secondBitDescriptionMap = [(2, "twenty"), (3, "thirty"), (4, "forty"), (5, "fifty"), (6, "sixty"), (7, "seventy"), (8, "eighty"), (9, "ninety")]

lookupWithDefault m n = case lookup n m of
      Nothing -> ""
      Just x -> x

num2Description :: Integer -> [Char]
num2Description n
  | n < 20 = lookupWithDefault numDescriptionMap n 
  | n < 100 = lookupWithDefault secondBitDescriptionMap (quot n 10) ++ "-" ++ lookupWithDefault numDescriptionMap (mod n 10)
  | n < 1000 = lookupWithDefault numDescriptionMap (quot n 100) ++ " hundred" ++ case (mod n 100) of
      m | m > 0 -> " and " ++ num2Description (mod n 100)
      m | otherwise -> ""


res = 11 + (sum $ map (length . filter (\x -> x /= '-' && x /= ' ')) $ map num2Description [1..999])
