import qualified Data.Heap as H
import qualified Data.HashMap.Lazy as M
import qualified Data.Set as S

import Data.Char (isLower, isUpper, toLower)

import Debug.Trace (traceShow, traceShowId)

type Point = (Int, Int)

type MinHeap = H.MinPrioHeap Int (S.Set Point, S.Set Char)

toMap :: [String] -> M.HashMap Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighbors (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

bfs :: Point -> M.HashMap Point (S.Set Char) -> M.HashMap Point Char -> M.HashMap Point (S.Set Char) -> Int -> M.HashMap (Point, Point) (Int, S.Set Char)
bfs start ps m seen d =
    if M.null ps then M.empty else M.union keys $ bfs start ns' m seen' (d+1)
    where ns = M.unions $ map (uncurry getNeighbors) $ M.toList $ ps
          ns' = M.difference ns seen
          seen' = M.union seen ns'

          keys = M.fromList $ M.elems $ M.mapWithKey resultForm $ M.filterWithKey isKey ns'

          getNeighbors p s = M.fromList $ map addBlocks ns
            where ns = filter canAccess $ neighbors p
                  addBlocks n = (n, s')
                    where c = m M.! n
                          s' = if isUpper c then S.insert (toLower c) s else s

          canAccess p = c /= '#' && not (p `M.member` seen)
            where c = m M.! p

          isKey p _ = isLower c
            where c = m M.! p

          resultForm p s = ((start, p), (d+1, s))

shortestPath :: Int -> M.HashMap Point Char -> M.HashMap (Point, Point) (Int, S.Set Char) -> MinHeap -> S.Set (S.Set Point, S.Set Char) -> Int
shortestPath pb m dists h seen
    | d > pb && traceShow (d, H.size h) False = undefined
    | S.null s = d
    | (ps, s) `S.member` seen = shortestPath d m dists h'' seen
    | not $ S.null $ S.filter (\(ps', s') -> ps == ps' && s' `S.isSubsetOf` s) seen = shortestPath d m dists h'' seen
    | otherwise = shortestPath d m dists h' seen'
    where Just ((d, (ps, s)), h'') = H.view h

          h' = S.foldl reachableSplit h'' ps
          seen' = S.insert (ps, s) seen

          reachableSplit h p = h'
            where ps' = S.delete p ps
                  ns = M.filterWithKey reachable dists
                  h' = M.foldrWithKey addToHeap h ns

                  reachable (from, to) (_, ts) =
                    from == p && c `S.member` s && S.null (S.intersection s ts)
                    where c = m M.! to

                  addToHeap :: (Point, Point) -> (Int, S.Set Char) -> MinHeap -> MinHeap
                  addToHeap (_, to) (d', ts) h = H.insert (d+d', (S.insert to ps', S.delete c s)) h
                    where c = m M.! to

add (x, y) (x', y') = (x+x', y+y')

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let [startPos] = M.keys $ M.filter (== '@') input
    let startPos' = [add startPos (dx, dy) | dx <- [-1,1], dy <- [-1,1]]

    let changes = (map (\p -> (p, '#')) $ startPos : neighbors startPos)
               ++ (map (\p -> (p, '@')) startPos')

    let realMap = M.union (M.fromList changes) input

    let keyMap = M.filter isLower input
    let dists = M.unions $ map (\p -> bfs p (M.singleton p S.empty) realMap (M.singleton p S.empty) 0) (startPos' ++ M.keys keyMap)

    let initialHeap = H.singleton (0, (S.fromList startPos', S.fromList $ M.elems keyMap))
    print $ shortestPath 0 input dists initialHeap S.empty
