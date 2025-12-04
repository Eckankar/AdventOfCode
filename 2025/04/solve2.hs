import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighbors (x, y) = [ (x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]

fixPoint f v = if v == v' then v else fixPoint f v'
    where v' = f v

accessible :: M.Map Point Char -> Point -> Bool
accessible m p = (<= 3) $ length $ filter (== Just '@') nsvs
    where ns   = neighbors p
          nsvs = map (flip M.lookup m) ns

removeAccessibleRolls :: M.Map Point Char -> M.Map Point Char
removeAccessibleRolls m = M.withoutKeys m aKeys
    where aKeys = S.fromList $ filter (accessible m) $ M.keys $ M.filter (== '@') m

countRolls = length . filter (== '@') . M.elems

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents
    
    let finalState = fixPoint removeAccessibleRolls input
    print $ countRolls input - countRolls finalState