import qualified Data.Map as M
import qualified Data.Set as S

import Data.Char (isUpper)
import Data.Maybe (maybeToList)
import Data.Tuple (swap)

type Point = (Integer, Integer)
data Side = Inner | Outer
    deriving (Show, Eq, Ord)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighbors (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

add (x, y) (x', y') = (x+x', y+y')
sub (x, y) (x', y') = (x-x', y-y')

flipSide Inner = Outer
flipSide Outer = Inner

findPortals m = (M.fromList pns, M.fromList $ map swap pns)
    where m' = M.filterWithKey hasPortal m
          pns  = [ ((portalName p, pType p), p) | p <- M.keys m' ]

          hasPortal p '.' = not $ null $ portalLetterNeighbors p
          hasPortal _ _ = False

          xmax = maximum $ map fst $ M.keys m
          ymax = maximum $ map snd $ M.keys m
          pType (x, y) = if x == 2 || x == xmax - 2 || y == 2 || y == ymax - 2 then Outer else Inner

          portalLetterNeighbors = filter (isUpper . (\p -> M.findWithDefault '#' p m)) . neighbors

          portalName p = S.fromList $ map (m M.!) [n1, n2]
            where [n1] = portalLetterNeighbors p
                  n2 = add n1 (sub n1 p)

bfs :: M.Map Point Char -> M.Map (S.Set Char, Side) Point -> M.Map Point (S.Set Char, Side)
                        -> S.Set (Point, Int) -> (Point, Int) -> S.Set (Point, Int) -> Int
bfs m portP pointP froms to seen =
    if S.member to froms then 0 else 1 + bfs m portP pointP ns' to seen'
    where ns    = S.fromList $ concatMap neighborsWithPortals froms
          ns'   = S.difference ns seen
          seen' = S.union seen ns'

          portalNeighbor (p, d) = do
            (n, s) <- M.lookup p pointP
            p' <- M.lookup (n, flipSide s) portP
            return (p', if s == Outer then d-1 else d+1)

          neighborsWithPortals (p, d) = filter (\(p', d') -> d' >= 0 && M.findWithDefault '#' p m == '.') ns
            where ns = maybeToList (portalNeighbor (p, d)) ++ (map (\p' -> (p', d)) $ neighbors p)

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents
    let (portPoint, pointPort) = findPortals input

    let start = portPoint M.! (S.singleton 'A', Outer)
    let end   = portPoint M.! (S.singleton 'Z', Outer)
    print $ bfs input portPoint pointPort (S.singleton (start, 0)) (end, 0) (S.singleton (start, 0))
