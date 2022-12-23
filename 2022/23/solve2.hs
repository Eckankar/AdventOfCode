import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe (catMaybes)

type Point = (Int, Int)

findElves :: [String] -> [Point]
findElves = map fst . filter ((== '#') . snd) .
              concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighbors (x, y) = S.fromList [ (x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0) ]

neighborsTo (dx, 0) (x, y) = S.fromList [ (x+dx, y+dy) | dy <- [-1..1] ]
neighborsTo (0, dy) (x, y) = S.fromList [ (x+dx, y+dy) | dx <- [-1..1] ]

add (x, y) (x', y') = (x+x', y+y')

countElements = foldr (M.alter inc) M.empty
    where inc Nothing  = Just 1
          inc (Just v) = Just (v+1)

takeTurn :: [Point] -> [Point] -> ([Point], [Point])
takeTurn elves ds@(d:dd) = (newPos', dd ++ [d])
    where newPos = map moveElf elves
          newPos' = map (uncurry realizeMove) newPos
          moveCounts = countElements $ map snd newPos

          realizeMove p p' | moveCounts M.! p' == 1 = p'
                           | otherwise              = p

          elfPositions = S.fromList elves

          moveElf p = (p, p')
            where p' = if noNeighbors then p
                       else if null freeNeighbors then p
                       else add p fd
                  noNeighbors = S.null $ S.intersection elfPositions $ neighbors p
                  freeNeighbors = catMaybes $ map checkNeighborTo ds
                  (fd:_) = freeNeighbors
                  checkNeighborTo d =
                    if S.null $ S.intersection elfPositions $ neighborsTo d p
                    then Just d else Nothing

bounds :: [Point] -> (Point, Point)
bounds es = ((minX, minY), (maxX, maxY))
    where (minX, maxX) = minMax $ map fst es
          (minY, maxY) = minMax $ map snd es
          minMax (e:es) = foldr (\a (lo, hi) -> (min a lo, max a hi)) (e,e) es

draw es bs = [ [ if (x, y) `S.member` ess then '#' else '.' | y <- [minY .. maxY] ] | x <- [minX .. maxX ] ]
    where ess = S.fromList es
          ((minX, minY), (maxX, maxY)) = bs

findFixedPoint :: [[Point]] -> Int
findFixedPoint = findFixedPoint' . map S.fromList
    where findFixedPoint' (s1:s2:ss)
            | s1 == s2  = 1
            | otherwise = 1 + findFixedPoint' (s2:ss)

main :: IO ()
main = do
    input <- fmap lines getContents
    let elves = findElves input
    let moves = map fst $ iterate (uncurry takeTurn) (elves, [(-1,0), (1,0), (0,-1), (0,1)])

    print $ findFixedPoint moves
