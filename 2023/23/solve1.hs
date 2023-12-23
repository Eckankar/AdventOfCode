import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighborDirs = [ (1,0), (-1,0), (0,1), (0,-1) ]

isCompatible '.' _ = True
isCompatible 'v' (1, 0) = True
isCompatible '^' (-1, 0) = True
isCompatible '<' (0, -1) = True
isCompatible '>' (0, 1) = True
isCompatible _ _ = False

add (x, y) (x', y') = (x+x', y+y')

longestPath :: M.Map Point Char -> [(Point, S.Set Point)] -> Int -> Int -> Point -> Int
longestPath _ [] _ bestRound _ = bestRound
longestPath m active curRound bestRound endPoint =
    longestPath m active' (curRound+1) bestRound' endPoint
    where bestRound' = if not $ null $ filter ((== endPoint) . fst) active then curRound else bestRound
          active' = concatMap (uncurry stepPath) active 

          stepPath p seen = map (\d -> (add d p, seen')) validDirs
            where validDirs = filter checkTile neighborDirs
                  checkTile d = M.member p' m && isCompatible (m M.! p') d && not (S.member p' seen)
                    where p' = add d p
                  seen' = S.insert p seen

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let startingPoint = minimum $ M.keys $ M.filter (== '.') input
    let endPoint = maximum $ M.keys $ M.filter (== '.') input

    print $ longestPath input [(startingPoint, S.empty)] 0 0 endPoint
