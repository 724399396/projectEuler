import Numbers

res = maximum [bitSum|a <- [1..100], b <- [1..100], let bitSum = sum $ num2Bits $ a ^ b]
