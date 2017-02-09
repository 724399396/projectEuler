baseExtract :: Integer -> Integer -> [Integer]
baseExtract x y = reverse $ help x y
  where help 0 b = []
        help a b = (rem a b) : (help (quot a b) b)

base10Extract a = baseExtract a 10
base2Extract a = baseExtract a 2

palindrome x = reverse x == x

res = sum $ filter (\x -> (palindrome $ base10Extract x) && (palindrome $ base2Extract x)) [1..1000000]
