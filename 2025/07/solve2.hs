import qualified Data.Map as M

import Data.List (partition, sortBy)
import Data.Maybe (fromMaybe)

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

findSplits :: M.Map Point Char -> M.Map Point Int
findSplits im = foldl compute initM pOrder
    where pOrder' = sortBy (\(x1, y1) (x2, y2) -> compare (x2, -y2) (x1, -y1)) $ M.keys im
          highCoord = maximum $ map fst pOrder'
          (highCoords, pOrder) = partition ((== highCoord) . fst) pOrder'
          initM = M.fromList $ map (\p -> (p, 1)) highCoords
          compute :: M.Map Point Int -> Point -> M.Map Point Int
          compute m (x, y) = M.insert (x, y) v m
              where lp = fromMaybe 0 $ M.lookup (x+1, y-1) m
                    mp = fromMaybe 0 $ M.lookup (x+1, y  ) m
                    rp = fromMaybe 0 $ M.lookup (x+1, y+1) m
                    v = case M.lookup (x, y) im of
                              Just '.' -> mp
                              Just 'S' -> mp
                              Just '^' -> lp + rp
main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let [start] = M.keys $ M.filter (== 'S') input
    let splitM = findSplits input
    print $ splitM M.! start