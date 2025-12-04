import qualified Data.Map as M

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighbors (x, y) = [ (x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]

accessible m p = (<= 3) $ length $ filter (== Just '@') nsvs
    where ns   = neighbors p
          nsvs = map (flip M.lookup m) ns

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents
    
    let accessibleRolls = filter (accessible input) $ M.keys $ M.filter (== '@') input
    print $ length accessibleRolls