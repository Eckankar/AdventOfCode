import qualified Data.Map as M

import Data.List (transpose)

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

expand = transpose . expand' . transpose . expand'
    where expand' = concatMap (\r -> if all (== '.') r then [r, r] else [r])

dist (x, y) (x', y') = abs (x-x') + abs (y-y')

main :: IO ()
main = do
    input <- fmap lines getContents

    let input' = expand input
    let points = M.keys $ M.filter (== '#') $ toMap input'

    print $ sum [ dist p1 p2  | p1 <- points, p2 <- points, p1 < p2 ]
