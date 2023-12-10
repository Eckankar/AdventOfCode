import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe (isJust)

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

add (x, y) (x', y') = (x+x', y+y')
neighbors (x, y) = S.fromList [ (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]

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

expandMap m = M.union m $ M.fromList $ map (, '.') borderPoints
    where (minX, minY) = minimum $ M.keys m
          (maxX, maxY) = maximum $ M.keys m
          borderPoints = [ (x, y) | x <- [minX-1, maxX+1], y <- [minY..maxY] ] ++
                         [ (x, y) | y <- [minY-1, maxY+1], x <- [minY-1..maxY+1] ]

-- finds the loop, and replaces the starting tile with the correct one
findLoop :: M.Map Point Char -> Point -> (S.Set Point, M.Map Point Char)
findLoop m start = (loop, m')
    where [(Just loop, m')] = filter (\(v, m') -> isJust v) $ map ((\m' -> (findLoop' m' 0 S.empty $ S.singleton start, m')) . (\v -> M.insert start v m)) "|-LJ7F"
          findLoop' :: M.Map Point Char -> Int -> S.Set Point -> S.Set Point -> Maybe (S.Set Point)
          findLoop' m' n seen ps
            | S.size ps == 0                  = Nothing
            | S.size ps == 1 && n > 0         = Just seen'
            | not $ all ((== 2) . S.size) ns' = Nothing
            | otherwise = findLoop' m' (n+1) seen' ps'
                where seen' = S.union seen ps
                      ps' = flip S.difference seen $ S.unions ns'
                      ns' = S.map findNeighbor ps
                      findNeighbor p = S.filter (S.member p . pipeNeighbors m') ns
                            where ns = pipeNeighbors m' p

floodFill :: S.Set Point -> S.Set Point -> S.Set Point -> S.Set Point
floodFill seen valid ps
    | S.null ps = seen
    | otherwise = floodFill seen' valid ps'
        where ps'   = S.intersection valid $ flip S.difference seen $ S.unions $ S.map neighbors ps
              seen' = S.union seen ps

followSeam :: M.Map Point Char -> S.Set Point -> S.Set Point
followSeam m loop = traverse start (startX, startY+1) S.empty
    where start@(startX, startY) = minimum loop
          -- traverse in clockwise direction
          -- start must necessarily be an F
          traverse p@(px,py) p'@(px',py') outside
            | p' == start = outside
            | otherwise = traverse p' pNext outside'
                where [pNext] = S.toList $ S.filter (/= p) $ pipeNeighbors m p'
                      pNorms = S.map (add (- (py'-py), px'-px )) $ S.fromList [p, p']
                      outside' = S.difference (S.union outside pNorms) loop

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let input' = expandMap input
    let [start] = M.keys $ M.filter (== 'S') input'
    let (loop, input'') = findLoop input' start

    let outsideStart = followSeam input'' loop
    let outsidePoints = floodFill loop (M.keysSet input'') outsideStart
    print $ S.size $ S.difference (M.keysSet input'') outsidePoints
