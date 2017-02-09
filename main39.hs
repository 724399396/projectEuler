import Data.List

faster_pow n = map (^2) [0..] !! n

isRightAngle (a:b:c:[]) = a <= b && b < c && faster_pow a + faster_pow b == faster_pow c

res = head $ sortBy (\(_,l1) (_,l2) -> compare l2 l1) $ map (\x -> (x,length x)) $ group [l|l <- [1..1000], a <- [1..l], b <- [1..l], let c = l - a - b, c > 0, isRightAngle [a,b,c]]
