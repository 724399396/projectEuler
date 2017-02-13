import Control.Monad

px = map p [1..]
  where
    p n = n*(3*n-1) `quot` 2

isPentagon = ap ((.last).(==)) $ flip takeWhile px . flip (<=)

res = head $ [p2-p1|p1 <- px, p2 <- (dropWhile (<=p1) px), isPentagon (p1+p2), isPentagon(p2-p1)]
