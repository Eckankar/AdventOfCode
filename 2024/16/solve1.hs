import qualified Data.Heap as H
import qualified Data.Map as M

import Data.Maybe (fromJust)

type Point = (Int, Int)

type MinHeap = H.HeapT (H.Prio H.FstMinPolicy (Int, (Point, Point))) (Point, Point)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((j, i), c)) $ zip [0..] r) . zip [0..]

add (x, y) (x', y') = (x+x', y+y')
rot (x, y) = (-y, x)
invRot (x, y) = (y, -x)

neighbors p d = [ ((add p d, d), 1), ((p, rot d), 1000), ((p, invRot d), 1000) ]

shortestPath :: M.Map Point Char -> M.Map (Point, Point) Int -> MinHeap -> Point -> Maybe Int
shortestPath m dm h e | H.isEmpty h = Nothing
shortestPath m dm h e
    | p == e               = Just v
    | (p, d) `M.member` dm = shortestPath m dm  h' e
    | otherwise            = shortestPath m dm' h' e
    where Just ((v, (p, d)), h'') = H.view h
          ns  = filter (\((n, d'), _) -> not ((n, d') `M.member` dm) && (m M.! n /= '#')) $ neighbors p d
          h'  = foldr (\((n, d'), v') -> H.insert (v + v', (n, d'))) h'' ns
          dm' = M.insert (p, d) v dm

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let startIdx = head $ M.keys $ M.filter (== 'S') input
    let endIdx   = head $ M.keys $ M.filter (== 'E') input

    let initQueue = H.fromList [ (0, (startIdx, (1, 0))) ]

    print $ fromJust $ shortestPath input M.empty initQueue endIdx
