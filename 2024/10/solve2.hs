import qualified Data.Set as S
import qualified Data.Map as M

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((j, i), c)) $ zip [0..] r) . zip [0..]

neighbors (x, y) = S.fromList [ (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]

increaseBy m Nothing  = Just m
increaseBy m (Just n) = Just $ n+m

backtrackToStarts m ends n = if n == 0 then ends' else backtrackToStarts m ends' (n-1)
    where ends' = foldr addPoint M.empty $ M.keys ends

          addPoint p ends' = foldr (M.alter (increaseBy v)) ends' ns
              where ns = M.keys $ M.filter (== n) $ M.restrictKeys m (neighbors p)
                    v = ends M.! p

main :: IO ()
main = do
    m <- fmap (M.map ((read :: String -> Int) . (:[])) . M.filter (/= '.') . toMap . lines) getContents

    let ends = M.map (const 1) $ M.filter (== 9) m
    print $ sum $ M.elems $ backtrackToStarts m ends 8
