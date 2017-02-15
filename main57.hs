import Data.Ratio
import Numbers

fast_div_num_list n = scanl (\acc _ -> 2 + 1 / acc) (2 % 1) [1..] !! (n-1)

expanding n = 1 + 1 / fast_div_num_list n

numeratorBitsExceedsDenominator ratio = let nLength = length $ num2Bits $ numerator ratio
                                            dLength = length $ num2Bits $ denominator ratio
                                        in nLength > dLength

res = length $ filter numeratorBitsExceedsDenominator $  map expanding [1..1000]
