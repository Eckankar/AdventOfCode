import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

add (x, y) (x', y') = (x+x', y+y')

followBeams m [] seen = S.map fst seen
followBeams m s seen = followBeams m s' seen'
    where s' = S.toList $ flip S.difference seen $ S.fromList $ filter (flip M.member m . fst) $ concatMap move s
          seen' = S.union seen $ S.fromList s

          move (p, d) = move' p d $ m M.! p

          move' p d '|' | d == (0, 1) || d == (0, -1) = concatMap (\d' -> move' p d' '.') [(-1, 0), (1, 0)]
          move' p d '-' | d == (1, 0) || d == (-1, 0) = concatMap (\d' -> move' p d' '.') [(0, -1), (0, 1)]
          move' p (dx, dy) '/'  = move' p (-dy, -dx) '.'
          move' p (dx, dy) '\\' = move' p ( dy,  dx) '.'
          move' p d _ = [ (add p d, d) ]

showBoard ps = putStrLn $ unlines $ [ [ if S.member (x, y) ps then '#' else '.' | y <- [minY .. maxY] ] | x <- [minX .. maxX] ]
    where minX = minimum $ S.map fst ps
          maxX = maximum $ S.map fst ps
          minY = minimum $ S.map snd ps
          maxY = maximum $ S.map snd ps

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let initialBeams = [((0, 0), (0, 1))]

    let res = followBeams input initialBeams S.empty

    showBoard res
    print $ S.size res
