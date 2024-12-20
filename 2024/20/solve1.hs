import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe (fromJust)

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((j, i), c)) $ zip [0..] r) . zip [0..]

add (x, y) (x', y') = (x+x', y+y')

neighbors p = [ add p d | d <- [ (0,1), (0,-1), (1,0), (-1,0) ] ]

bfs :: M.Map Point Char -> M.Map Point Int -> [Point] -> Int -> M.Map Point Int
bfs m seen [] s = seen
bfs m seen fs s = bfs m seen' fs' (s+1)
    where fs' = filter (\p -> m M.! p == '.') $ S.toList $ S.difference (S.fromList $ concatMap neighbors fs) $ M.keysSet seen
          seen' = M.union seen $ M.fromList $ [ (p, s) | p <- fs' ]

findCheats :: M.Map Point Int -> Int -> [(Point, Point, Int)]
findCheats m d = concatMap (uncurry findCheat) $ M.toList m
    where n2 = S.fromList . concatMap neighbors . neighbors

          findCheat :: Point -> Int -> [(Point, Point, Int)]
          findCheat p v = map (\(p', v') -> (p, p', v')) cs
            where cs = M.toList $ M.filter (> 0) $ M.map (\v' -> v'-v-2) $ M.restrictKeys m $ n2 p

increaseBy m Nothing  = Just m
increaseBy m (Just n) = Just $ n+m

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let startIdx = head $ M.keys $ M.filter (== 'S') input
    let endIdx   = head $ M.keys $ M.filter (== 'E') input

    let input' = M.map (\c -> if c /= '#' then '.' else '#') input

    let distMap = bfs input' (M.fromList [(startIdx, 1)]) [startIdx] 2

    let cheats = findCheats distMap 2

    --let cheatCounts = foldr (M.alter (increaseBy 1)) M.empty $ map (\(_, _, d) -> d) $ cheats
    print $ length $ filter (\(_, _, v') -> v' >= 100) $ findCheats distMap 2
