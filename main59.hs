import Data.Bits

decrypt src key = xor src key

possibleKeys = map ord ['a'..'z']
