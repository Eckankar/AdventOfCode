import qualified Data.Map as M
import qualified Data.Set as S


type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

simulate :: M.Map Point Char -> Point -> S.Set Point
simulate m sp = simulate' S.empty sp
    where simulate' :: S.Set Point -> Point -> S.Set Point
          simulate' seen (x, y)
            | S.member (x, y) seen = seen
            | otherwise = case M.lookup (x, y) m of
                Just 'S' -> simulate' seen (x+1, y)
                Just '.' -> simulate' seen (x+1, y)
                Just '^' -> simulate' seen' (x+1, y+1)
                    where seen'' = S.insert (x, y) seen
                          seen' = simulate' seen'' (x+1, y-1)
                _ -> seen

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let [start] = M.keys $ M.filter (== 'S') input
    print $ S.size $ simulate input start