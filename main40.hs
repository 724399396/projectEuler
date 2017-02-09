digit = reverse . help
  where
    help 0 = []
    help n = (mod n 10) : help (quot n 10)

res = let from0 = concat $ map digit [0..]
          indexN n = from0 !! (n-1)
      in
        (indexN 1) * (indexN 10) * (indexN 100) * (indexN 1000) * (indexN 10000) *(indexN 100000) * (indexN 1000000)
