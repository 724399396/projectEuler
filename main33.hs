import Data.List
import Data.Ratio

type Fraction = (Int,Int)
type IntBit = (Int,Int)

fra2Ratio (a,b) = a % b

int2Bit :: Int -> IntBit
int2Bit x = (quot x 10, mod x 10)

intBitExistSame ::  Int -> Int -> Bool
intBitExistSame a b = let (ah,al) = int2Bit a
                          (bh,bl) = int2Bit b
                      in
                        ah == bh || ah == bl || al == bh || al == bl

same a b c d
  | a == c = a
  | a == d = a
  | b == c = b
  | b == d = b

rmOneBit :: Fraction -> Fraction
rmOneBit (a,b) = let (ah,al) = int2Bit a
                     (bh,bl) = int2Bit b
                     s  = same ah al bh bl
                     fa = if (ah == s) then al else ah
                     fb = if (bh == s) then bl else bh
                 in
                   (fa, fb)

res = product $ map fra2Ratio [(x,y)|x <- [10..99], y <- [(x+1)..99], y > x, mod x 10 /= 0, mod y 10 /= 0, intBitExistSame x y, let (a,b) = rmOneBit (x, y), b /= 0, fra2Ratio (a,b) == fra2Ratio (x,y)]
