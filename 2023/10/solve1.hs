import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

add (x, y) (x', y') = (x+x', y+y')
neighbors (x, y) = [ (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]

pipeNeighbors m p =
    S.fromList $ map (add p) $ case t of
        '|' -> [ (-1, 0), (1, 0) ]
        '-' -> [ (0, -1), (0, 1) ]
        'L' -> [ (-1, 0), (0, 1) ]
        'J' -> [ (-1, 0), (0, -1) ]
        '7' -> [ (1, 0), (0, -1) ]
        'F' -> [ (1, 0), (0, 1) ]
        'S' -> [ (0, 1), (1, 0), (0, -1), (-1, 0) ]
        _   -> []
    where t = M.findWithDefault '.' p m

findLoop :: M.Map Point Char -> Point -> Int
findLoop m start = findLoop' 0 S.empty $ S.singleton start
    where findLoop' :: Int -> S.Set Point -> S.Set Point -> Int
          findLoop' n seen ps
            | S.size ps == 1 && n > 0 = n
            | otherwise = findLoop' (n+1) seen' ps'
                where seen' = S.union seen ps
                      ps' = flip S.difference seen $ S.unions $ S.map findNeighbor ps
                      findNeighbor p = S.filter (S.member p . pipeNeighbors m) ns
                            where ns = pipeNeighbors m p

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let [start] = M.keys $ M.filter (== 'S') input
    print $ findLoop input start
