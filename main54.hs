import Data.Char (digitToInt)
import Data.List

type Poker = (Int,Char)
type HandPoker = [Poker]

poker2Num 'T' = 10
poker2Num 'J' = 11
poker2Num 'Q' = 12
poker2Num 'K' = 13
poker2Num 'A' = 14
poker2Num x = digitToInt x

data PokersOrder = HighCard [Int] | OnePair [Int] | TwoPairs [Int] | ThreeOfAKind Int Int Int | Straight Int | Flush [Int] | FullHouse Int Int | FourOfAKind Int Int | StraightFlush Int | RoyalFlush deriving (Eq,Ord,Show)

allSame xs = all (== head xs) xs

consecutive ns = fst $ foldl' step (True,head ns) (tail ns)
  where step (acc,lastValue) x = (acc && lastValue + 1 == x, x)

pairAllSame (xs,ys) = allSame xs && allSame ys

groupThenSortLengthOrd = map head . sortBy (\x y -> compare (length y) (length x) `mappend` (compare (head y) (head x))) . group

poker2Order (ns,cs)
  | ns == [10..14] && allSame cs = RoyalFlush
  | consecutive ns && allSame cs = StraightFlush (last ns)
  | allSame (tail ns) = FourOfAKind (ns !! 4) (ns !! 0)
  | allSame (init ns) = FourOfAKind (ns !! 0) (ns !! 4)
  | pairAllSame (splitAt 2 ns) = FullHouse (ns !! 4) (ns !! 0)
  | pairAllSame (splitAt 3 ns) = FullHouse (ns !! 0) (ns !! 4)
  | allSame cs = Flush (reverse ns)
  | consecutive ns = Straight (last ns)
  | allSame (take 3 ns) = ThreeOfAKind (ns !! 0) (ns !! 4) (ns !! 3)
  | allSame (take 3 $ drop 1 ns) = ThreeOfAKind (ns !! 1) (ns !! 4) (ns !! 0)
  | allSame (take 3 $ drop 2 ns) = ThreeOfAKind (ns !! 2) (ns !! 1) (ns !! 0)
  | (length $ group ns) == 3 = TwoPairs $ groupThenSortLengthOrd ns
  | (length $ group ns) == 4 = OnePair $ groupThenSortLengthOrd ns
  | otherwise = HighCard (reverse ns)

str2PokerPairs :: String -> [(HandPoker,HandPoker)]
str2PokerPairs = map (oneLineToTwoHandsPokers.map str2Poker.words) .lines

str2Poker :: String -> Poker
str2Poker str = (poker2Num $ str !! 0, str !! 1)

pokerSort :: (Int,Char) -> (Int,Char) -> Ordering
pokerSort (n1,_) (n2,_) = n1 `compare` n2

oneLineToTwoHandsPokers :: [Poker] -> (HandPoker,HandPoker)
oneLineToTwoHandsPokers xs = case splitAt 5 xs of
                               (p1,p2) -> (sortBy pokerSort p1, sortBy pokerSort p2)

pokerCompare (p1,p2) = compare (poker2Order $ unzip p1) (poker2Order $ unzip p2)

str2Ordering = map pokerCompare . str2PokerPairs
    
main :: IO ()
main = do
  content <- readFile "p054_poker.txt"
  return ()
  (putStrLn . show . length . filter (==GT) . str2Ordering) content
  
  
