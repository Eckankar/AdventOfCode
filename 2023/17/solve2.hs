import qualified Data.Heap as H
import qualified Data.Map as M

import Data.Maybe (fromJust)
import Data.Tuple (swap)

type Point = (Int, Int)

type WorldMap = M.Map Point Int
type MinHeap = H.HeapT (H.Prio H.FstMinPolicy (Int, (Point, Point))) (Point, Point)

toMap :: [String] -> M.Map Point Int
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), read [c])) $ zip [0..] r) . zip [0..]

sumFrom m p p' | p == p' = 0
sumFrom m (x, y) (x', y')
    | y == y' && x < x' = m M.! (x+1, y) + sumFrom m (x+1, y) (x', y')
    | y == y' && x > x' = m M.! (x-1, y) + sumFrom m (x-1, y) (x', y')
    | x == x' && y < y' = m M.! (x, y+1) + sumFrom m (x, y+1) (x', y')
    | x == x' && y > y' = m M.! (x, y-1) + sumFrom m (x, y-1) (x', y')

neighbours p d = map (add p . flip mul d) [ n | n <- [-10..10], n <= -4 || n >= 4 ]
mul n (x, y) = (n*x, n*y)
add (x, y) (x', y') = (x+x', y+y')

shortestPath :: WorldMap -> M.Map (Point, Point) Int -> MinHeap -> Point -> Maybe Int
shortestPath m dm h e | H.isEmpty h = Nothing
shortestPath m dm h e
    | p == e               = Just v
    | (p, d) `M.member` dm = shortestPath m dm  h' e
    | otherwise            = shortestPath m dm' h' e
    where Just ((v, (p, d)), h'') = H.view h
          d'  = swap d
          ns  = filter (\n -> not ((n, d') `M.member` dm) && (n `M.member` m)) $ neighbours p d'
          h'  = H.filter ((/= (p, d)) . snd) $ foldr (\n -> H.insert (v + sumFrom m p n, (n, d'))) h'' ns
          dm' = M.insert (p, d) v dm


main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let startIdx = (0, 0)
    let endIdx = maximum $ M.keys input

    let initQueue = H.fromList [ (0, ((0,0), (1,0))), (0, ((0,0), (0, 1))) ]

    print $ fromJust $ shortestPath input M.empty initQueue endIdx
