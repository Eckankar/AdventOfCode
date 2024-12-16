import qualified Data.Heap as H
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe (fromJust, isJust)

type Point = (Int, Int)

type MinHeap = H.MinPrioHeap Int (Point, Point, S.Set Point)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((j, i), c)) $ zip [0..] r) . zip [0..]

add (x, y) (x', y') = (x+x', y+y')
rot (x, y) = (-y, x)
invRot (x, y) = (y, -x)

neighbors p d = [ ((add p d, d), 1), ((p, rot d), 1000), ((p, invRot d), 1000) ]

shortestPath :: M.Map Point Char -> M.Map (Point, Point) Int -> MinHeap -> Point -> [S.Set Point] -> Maybe Int -> [S.Set Point]
shortestPath m dm h e pss spl | H.isEmpty h = undefined
shortestPath m dm h e pss spl
    | isJust spl && fromJust spl < v = pss
    | p == e               = shortestPath m dm h' e (ps:pss) (Just v)
    | (p, d) `M.member` dm = shortestPath m dm  h' e pss spl
    | otherwise            = shortestPath m dm' h' e pss spl
    where Just ((v, (p, d, ps)), h'') = H.view h
          ns  = filter (\((n, d'), _) -> not ((n, d') `M.member` dm) && (m M.! n /= '#')) $ neighbors p d
          h' :: MinHeap
          h'  = foldr (\((n, d'), v') -> H.insert (v + v', (n, d', S.insert n ps))) h'' ns
          dm' = M.insert (p, d) v dm

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let startIdx = head $ M.keys $ M.filter (== 'S') input
    let endIdx   = head $ M.keys $ M.filter (== 'E') input

    let initQueue = H.fromList [ (0, (startIdx, (1, 0), S.fromList [startIdx])) ]

    let visitedTiles = S.unions $ shortestPath input M.empty initQueue endIdx [] Nothing
    print $ S.size visitedTiles
