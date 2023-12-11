import qualified Data.Map as M

import Data.List (transpose)

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

dist (x, y) (x', y') = abs (x-x') + abs (y-y')

expand n emptyRows emptyCols ps = map expand' ps
    where expand' (x, y) = (x + dx, y + dy)
            where dx = sum [ n-1 | (x', c) <- emptyRows, x' < x && c ]
                  dy = sum [ n-1 | (y', c) <- emptyCols, y' < y && c ]

main :: IO ()
main = do
    input <- fmap lines getContents

    let emptyRows = zip [0..] $ map (all (== '.')) input
    let emptyCols = zip [0..] $ map (all (== '.')) $ transpose input

    let points = expand 1000000 emptyRows emptyCols $ M.keys $ M.filter (== '#') $ toMap input

    print $ sum [ dist p1 p2  | p1 <- points, p2 <- points, p1 < p2 ]
