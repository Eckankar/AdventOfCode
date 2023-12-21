import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Integer, Integer)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighbors (x, y) = S.fromList [ (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]

bfs _ _ 65 _ reachable = reachable
bfs active seen n walkable reachable = bfs active' seen' (n+1) walkable reachable'
    where active' = S.difference (S.intersection walkable $ S.unions $ S.map neighbors active) seen
          seen' = S.union seen active
          reachable' = if n `rem` 2 == 0 then S.union reachable active else reachable

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents
    
    let [startingPoint] = M.keys $ M.filter (== 'S') input
    let walkable = M.keysSet $ M.filter (/= '#') input

    print $ S.size $ bfs (S.singleton startingPoint) S.empty 0 walkable S.empty
