import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((j, i), c)) $ zip [0..] r) . zip [0..]

neighbors (x, y) = S.fromList [ (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]

findRegion :: M.Map Point Char -> Point -> S.Set Point
findRegion m p = findRegion' sp sp
    where sp = S.singleton p
          findRegion' seen ps
            | S.null ps = seen
            | otherwise = findRegion' seen' ns'
                 where ns = S.difference (S.unions $ S.map neighbors ps) seen
                       ns' = M.keysSet $ M.filter (== pc) $ M.restrictKeys m ns
                       seen' = S.union seen ns'
          pc = m M.! p

findAllRegions :: M.Map Point Char -> [(S.Set Point, Char)]
findAllRegions m =
    case M.lookupMin m of
        Nothing     -> []
        Just (p, c) -> (rs, c) : findAllRegions m'
            where rs = findRegion m p
                  m' = M.withoutKeys m rs

add (x, y) (x', y') = (x+x', y+y')
sub (x, y) (x', y') = (x-x', y-y')
mul n (x, y) = (n*x, n*y)
rot (x, y) = (y, -x)

perimeter :: S.Set Point -> Int
perimeter s = sum $ map (\p -> S.size $ S.difference (neighbors p) s) $ S.toList s

sides :: S.Set Point -> Int
sides ps = sides os
    where os = S.fromList [ (n, sub n p) | p <- S.toList ps, n <- S.toList $ S.difference (neighbors p) ps ]
          sides os'
            | S.null os' = 0
            | otherwise = 1 + sides os''
                where (e, d) = S.findMin os'
                      ls = takeWhile (`S.member` os') $ [ (add e (mul n (rot d)), d) | n <- [1..] ]
                      rs = takeWhile (`S.member` os') $ [ (add e (mul n (rot (rot (rot d)))), d) | n <- [1..] ]
                      s = S.fromList $ (e, d) : ls ++ rs
                      os'' = S.difference os' s

main :: IO ()
main = do
    m <- fmap (toMap . lines) getContents

    let regions = findAllRegions m
    print $ sum $ map (\(rs, c) -> sides rs * S.size rs) regions
