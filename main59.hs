import Data.Bits
import Data.List
import Data.Char
import Control.Monad
import Data.List.Split

decrypt (src,key) = xor src key

possibleKeys = map (map ord) [[a,b,c]|a<-['a'..'z'],b<-['a'..'z'],c<-['a'..'z']]

cycleDecrypt _ [] = []
cycleDecrypt keys content = let (h,t) = splitAt (length keys) content
                            in (map (chr . decrypt) $ zip keys h) ++ cycleDecrypt keys t

analysis decryptContent = map (\x -> fromIntegral (length (filter (==x) decryptContent)) / fromIntegral (length decryptContent)) ['a'..'z']

third (_,_,x) = x
                          
main :: IO ()
main = do
  content <- readFile "p059_cipher.txt"
  let ordContent = map read $ splitOn "," content :: [Int]
  let decryptContent = take 3 $ sortOn (abs .((-) 0.127).flip (!!) 4 . analysis . third) $ map (\(dk,dc) -> (dk,sum $ map ord dc, dc))$ map (ap (,) $ flip cycleDecrypt ordContent) possibleKeys
  print decryptContent
