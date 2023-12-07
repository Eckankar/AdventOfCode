import qualified Data.Map as M

import Data.List (sort, sortBy)

data HandType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
    deriving (Eq, Ord, Show)

parseInputLine s = (hand, read bid)
    where (hand, _:bid) = break (== ' ') s

cardRank :: Char -> Int
cardRank 'A' = 14
cardRank 'K' = 13
cardRank 'Q' = 12
cardRank 'J' = 1
cardRank 'T' = 10
cardRank c   = read [c]

handType h1 =
    case M.findWithDefault 0 'J' cm of
        5 -> FiveKind
        4 -> FiveKind
        3 -> if cs == [2] then FiveKind else FourKind
        0 -> case sort $ M.elems $ count h1 of
                 [5] -> FiveKind
                 [1,4] -> FourKind
                 [2,3] -> FullHouse
                 [1,1,3] -> ThreeKind
                 [1,2,2] -> TwoPair
                 [1,1,1,2] -> OnePair
                 [1,1,1,1,1] -> HighCard
        otherwise -> maximum $ map handType $ replaceJokers h1
    where cm = count h1
          cs = sort $ M.elems $ M.filterWithKey (\k _ -> k /= 'J') cm

          replaceJokers :: [Char] -> [[Char]]
          replaceJokers ('J':hs) = concatMap (\h -> map (h :) $ replaceJokers hs) "AKQT98765432"
          replaceJokers (h:hs) = map (h :) $ replaceJokers hs
          replaceJokers [] = [[]]

count ls = foldr (M.alter (increaseBy 1)) M.empty ls

increaseBy m Nothing  = Just m
increaseBy m (Just n) = Just $ n+m

handCompare h1 h2 = compare (handType h1, h1r) (handType h2, h2r)
    where h1r = map cardRank h1
          h2r = map cardRank h2

main :: IO ()
main = do
    input <- fmap (map parseInputLine . lines) getContents

    let winningOrder = sortBy (\(h1, _) (h2, _) -> handCompare h1 h2) input
    print $ sum $ zipWith (\(_, b) r -> b*r) winningOrder [1..]
