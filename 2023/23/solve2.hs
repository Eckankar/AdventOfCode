import qualified Data.Map as M
import qualified Data.Set as S

import Data.Tuple.Extra (fst3, snd3)

import Debug.Trace (traceShow, traceShowId)

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighborDirs = [ (1,0), (-1,0), (0,1), (0,-1) ]

isCompatible '.' _ = True
isCompatible 'v' (1, 0) = True
isCompatible '^' (-1, 0) = True
isCompatible '<' (0, -1) = True
isCompatible '>' (0, 1) = True
isCompatible _ _ = False

add (x, y) (x', y') = (x+x', y+y')

reduceGraph :: M.Map Point Char -> [(Point, Point, Int, S.Set Point)] -> Point -> M.Map (Point, Point) Int -> M.Map (Point, Point) Int
reduceGraph _ [] _ rm  = rm
reduceGraph m active endPoint rm = reduceGraph m active' endPoint rm'
    where (active', rm') = foldr stepPath ([], rm) active 

          stepPath (p, lastPoi, lastPoiDist, seen) (as, rm) = (as', rm') 
            where as' = map (\d -> (add d p, lastPoi', lastPoiDist', seen')) validDirs ++ as
                  validDirs = filter checkTile neighborDirs
                  checkTile d = M.member p' m && isCompatible (m M.! p') d && not (S.member p' seen)
                    where p' = add d p
                  seen' = S.insert p seen
                  isPoi = p == endPoint || (null $ filter (== '.') $ map ((m M.!) . (add p)) validDirs)
                  lastPoi' = if isPoi then p else lastPoi
                  lastPoiDist' = if isPoi then 0 else lastPoiDist+1
                  rm' = if isPoi then M.insert (lastPoi, p) (lastPoiDist+1) rm else rm

longestPath :: M.Map (Point, Point) Int -> [(Point, Int, S.Set Point)] -> Point -> Int -> Int
longestPath m active endPoint bestRound | traceShow (length active, bestRound) False = undefined
longestPath _ [] _ bestRound = bestRound
longestPath m active endPoint bestRound = longestPath m active' endPoint bestRound'
    where active' = concatMap stepPath active
          bestRound' = maximum $ bestRound : map snd3 (filter ((== endPoint) . fst3) active)

          stepPath (p, d, seen) = map gotoNeighbor validNeighbors
            where graphNeighbors = map snd $ M.keys $ M.filterWithKey (\(p', _) _ -> p' == p) m
                  validNeighbors = filter (not . flip S.member seen) graphNeighbors
                  gotoNeighbor p' = (p', d + m M.! (p,p'), S.insert p seen)

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let startingPoint = minimum $ M.keys $ M.filter (== '.') input
    let endPoint = maximum $ M.keys $ M.filter (== '.') input

    let reducedGraph = reduceGraph input [(startingPoint, startingPoint, 0, S.empty)] endPoint M.empty
    let reducedGraph' = M.union reducedGraph $ M.mapKeys (\(p1, p2) -> (p2, p1)) reducedGraph

    print $ longestPath reducedGraph' [(startingPoint, -1, S.empty)] endPoint 0
