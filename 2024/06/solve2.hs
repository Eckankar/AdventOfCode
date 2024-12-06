import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((j, i), c)) $ zip [0..] r) . zip [0..]

extractGuard :: M.Map Point Char -> (Point, Point)
extractGuard m = (pos, decodeSymb symb)
    where [(pos, symb)] = M.toList $ M.filter (not . flip S.member (S.fromList "#.")) m
          decodeSymb '^' = ( 0, -1)
          decodeSymb 'v' = ( 0,  1)
          decodeSymb '<' = (-1,  0)
          decodeSymb '>' = ( 1,  0)

add (x, y) (x', y') = (x+x', y+y')

rotate (x, y) = (-y, x)

simGuard :: M.Map Point Char -> S.Set (Point, Point) -> (Point, Point) -> (M.Map Point Char, Bool)
simGuard m seen (p, d) =
    if ps' == Nothing || looping then (m', looping)
    else if ps' == Just '#' then simGuard m seen (p, rotate d)
    else simGuard m' seen' (p', d)
    where m' = M.insert p 'X' m
          p' = add p d
          ps' = M.lookup p' m
          seen' = S.insert (p, d) seen
          looping = (p, d) `S.member` seen

bounds es = ((minX, minY), (maxX, maxY))
    where (minX, maxX) = minMax $ map fst es
          (minY, maxY) = minMax $ map snd es
          minMax (e:es) = foldr (\a (lo, hi) -> (min a lo, max a hi)) (e,e) es

draw :: M.Map Point Char -> [String]
draw m = [ [ m M.! (x, y) | x <- [minX .. maxX] ] | y <- [minY .. maxY] ]
    where ((minX, minY), (maxX, maxY)) = bounds $ M.keys m

showMap m = do
    putStrLn ""
    mapM_ putStrLn $ draw m

main :: IO ()
main = do
    m <- fmap (toMap . lines) getContents

    showMap m
    let guard = extractGuard m

    let (m', False) = simGuard m S.empty guard
    showMap m'

    print $ length $ filter (\p -> snd $ simGuard (M.insert p '#' m) S.empty guard) $ M.keys $ M.filter (== 'X') m'
