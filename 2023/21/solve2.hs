import qualified Data.Map as M
import qualified Data.Set as S

import Data.Tuple.Extra (both)
import Math.Regression.Simple (quadratic, V3(..))

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighbors (x, y) = S.fromList [ (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]

bfs :: S.Set Point -> S.Set Point -> Int -> S.Set Point -> M.Map Point Int -> M.Map Point Int
bfs active _ _ _ dists | S.null active = dists
bfs active seen n walkable dists = bfs active' seen' (n+1) walkable dists'
    where active' = S.difference (S.intersection walkable $ S.unions $ S.map neighbors active) seen
          seen' = S.union seen active
          dists' = M.union dists $ M.fromList $ map (, n) $ S.toList active
    
add (x, y) (x', y') = (x+x', y+y')
sub (x, y) (x', y') = (x-x', y-y')
mul n (x, y) = (n*x, n*y)

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents
    
    let [startingPoint] = M.keys $ M.filter (== 'S') input
    let walkable = M.keysSet $ M.filter (/= '#') input

    let width = (1 +) $ maximum $ S.map fst $ walkable

    let targetSteps = 26501365
    let r = targetSteps `mod` width

    -- 131 (size of input) = 65*2 + 1
    -- 26501365 mod 131 = 65
    -- hmm, I wonder if it's only got full squares...

    let offsets = [ (x, y) | x <- [-4..4], y <- [-4..4] ]

    let expWalkable = S.unions $ map (\dk -> S.map (add $ mul width dk) walkable) offsets
    let sDists = bfs (S.singleton startingPoint) S.empty 0 expWalkable M.empty
    let origSDists = M.restrictKeys sDists walkable
    let subGridMap = M.fromList $ map (\p -> (p, M.mapKeys (`sub` (mul width p)) $ M.restrictKeys sDists $ S.map (add $ mul width p) walkable)) offsets

    let v0 = (0, sum [ m M.! (x, y) | let m = M.map (M.size . M.filter (<= (width * 0 + r)) . M.filter ((== 0) . (`mod` 2) . (+ (width*0+r)))) subGridMap, x <- [0], y <- [0] ])
    let v1 = (1, sum [ m M.! (x, y) | let m = M.map (M.size . M.filter (<= (width * 1 + r)) . M.filter ((== 0) . (`mod` 2) . (+ (width*1+r)))) subGridMap, x <- [-1..1], y <- [-1..1] ])
    let v2 = (2, sum [ m M.! (x, y) | let m = M.map (M.size . M.filter (<= (width * 2 + r)) . M.filter ((== 0) . (`mod` 2) . (+ (width*2+r)))) subGridMap, x <- [-2..2], y <- [-2..2] ])
    let v3 = (3, sum [ m M.! (x, y) | let m = M.map (M.size . M.filter (<= (width * 3 + r)) . M.filter ((== 0) . (`mod` 2) . (+ (width*3+r)))) subGridMap, x <- [-3..3], y <- [-3..3] ])
    let v4 = (4, sum [ m M.! (x, y) | let m = M.map (M.size . M.filter (<= (width * 4 + r)) . M.filter ((== 0) . (`mod` 2) . (+ (width*4+r)))) subGridMap, x <- [-4..4], y <- [-4..4] ])

    -- print $ [v0, v1, v2, v3, v4]
    -- plotting looks quadratic... worth a shot.
    -- https://www.wolframalpha.com/input?i=polynomial+fit+%5B%280%2C3885%29%2C%281%2C34700%29%2C%282%2C96215%29%2C%283%2C188430%29%2C%284%2C311345%29%5D

    let (V3 a b c) = quadratic (both fromIntegral) [v0, v1, v2]
    let x = fromIntegral $ targetSteps `div` width

    print (a, b, c)
    print $ round $ a * x*x + b * x + c
