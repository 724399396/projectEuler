import Data.Numbers.Primes
import Numbers

numConcat x y = let xBits = num2Bits x
                    yBits = num2Bits y
                in map bits2Num [xBits ++ yBits, yBits ++ xBits]
