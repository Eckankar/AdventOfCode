import qualified Data.Map as M
import qualified Data.Set as S

import Data.List (groupBy, sortBy)

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((j, i), c)) $ zip [0..] r) . zip [0..]

bounds es = ((minX, minY), (maxX, maxY))
    where (minX, maxX) = minMax $ map fst es
          (minY, maxY) = minMax $ map snd es
          minMax (e:es) = foldr (\a (lo, hi) -> (min a lo, max a hi)) (e,e) es

inBounds ((minX, minY), (maxX, maxY)) (x, y) =
    x >= minX && x <= maxX && y >= minY && y <= maxY

add (x, y) (x', y') = (x+x', y+y')

findAntinodes ib ps =
    S.unions $ 
    [
        S.fromList $ takeWhile ib ps1 ++ takeWhile ib ps2
        | (x1, y1) <- ps,
          (x2, y2) <- ps,
          (x1, y1) /= (x2, y2),
          let dx = x1-x2,
          let dy = y1-y2,

          let ps1 = iterate (add (dx, dy))   (x1, y1),
          let ps2 = iterate (add (-dx, -dy)) (x2, y2)
    ]

main :: IO ()
main = do
    m <- fmap (toMap . lines) getContents

    let mapBounds = bounds $ M.keys m

    let groups = map (map fst) $ groupBy (\(_, a) (_, b) -> a == b) $ sortBy (\(_, a) (_, b) -> compare a b) $ M.toList $ M.filter (/= '.') m

    let antinodes = S.filter (inBounds mapBounds) $ S.unions $ map (findAntinodes (inBounds mapBounds)) groups
    print $ S.size antinodes
