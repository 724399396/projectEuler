digit 0 = []
digit n = (mod n 10) : digit (quot n 10)

f mf 0 = 1
f mf n = n * mf (n-1)

f_map = map (f faster_f) [0..]

faster_f n = f_map !! n

digitFactorialSame x = let digits = digit x
                       in
                         length digits > 1 && x == (sum $ map faster_f digits )

res = filter digitFactorialSame [1..10000000]                          
