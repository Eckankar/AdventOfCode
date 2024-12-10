import qualified Data.Set as S
import qualified Data.Map as M

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((j, i), c)) $ zip [0..] r) . zip [0..]

neighbors (x, y) = S.fromList [ (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]

backtrackToStarts :: M.Map Point Int -> M.Map Point (S.Set Int) -> Int -> M.Map Point (S.Set Int)
backtrackToStarts m ends n =  if n == 0 then ends' else backtrackToStarts m ends' (n-1)
    where ends' = foldr addPoint M.empty $ M.keys ends

          addPoint p ends' = foldr (M.alter (updateReachable rs)) ends' ns
              where ns = M.keys $ M.filter (== n) $ M.restrictKeys m (neighbors p)
                    rs = ends M.! p

          updateReachable rs Nothing   = Just rs
          updateReachable rs (Just ps) = Just $ S.union rs ps

main :: IO ()
main = do
    m <- fmap (M.map ((read :: String -> Int) . (:[])) . M.filter (/= '.') . toMap . lines) getContents

    let ends = M.fromList $ flip zip [ S.singleton n | n <- [0..] ] $ M.keys $ M.filter (== 9) m
    let trailheads = backtrackToStarts m ends 8
    print $ sum $ map S.size $ M.elems trailheads
