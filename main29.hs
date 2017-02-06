import qualified Data.Set as S

res n = S.size $ S.fromList [x ^ y| x<-[2..(fromIntegral n)], y<-[2..(fromIntegral n)]]
