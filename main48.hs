selfPow n = n ^ n

res = sum $ map selfPow [1..1000]
