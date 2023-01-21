import qualified Data.Map as M
import qualified Data.Set as S

import Data.Char (isUpper)

type Point = (Integer, Integer)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighbors (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

add (x, y) (x', y') = (x+x', y+y')
sub (x, y) (x', y') = (x-x', y-y')

findPortals m = (M.fromListWith (++) pns, M.fromList pns')
    where m' = M.filterWithKey hasPortal m
          pns  = [ (portalName p, [p]) | p <- M.keys m' ]
          pns' = [ (p, portalName p) | p <- M.keys m' ]

          hasPortal p '.' = not $ null $ portalLetterNeighbors p
          hasPortal _ _ = False

          portalLetterNeighbors = filter (isUpper . (\p -> M.findWithDefault '#' p m)) . neighbors

          portalName p = S.fromList $ map (m M.!) [n1, n2]
            where [n1] = portalLetterNeighbors p
                  n2 = add n1 (sub n1 p)

bfs :: M.Map Point Char -> M.Map (S.Set Char) [Point] -> M.Map Point (S.Set Char) -> S.Set Point -> Point -> S.Set Point -> Int
bfs m portP pointP froms to seen =
    if S.member to froms then 0 else 1 + bfs m portP pointP ns' to seen'
    where ns    = S.fromList $ concatMap neighborsWithPortals froms
          ns'   = S.difference ns seen
          seen' = S.union seen ns'

          neighborsWithPortals p = ns
            where ns = filter (\n -> '.' == M.findWithDefault '#' n m) $ pn ++ neighbors p
                  pn = filter (/= p) $ M.findWithDefault [] pname portP
                  pname = M.findWithDefault S.empty p pointP

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents
    let (portPoint, pointPort) = findPortals input

    let [start] = portPoint M.! (S.singleton 'A')
    let [end]   = portPoint M.! (S.singleton 'Z')
    print $ bfs input portPoint pointPort (S.singleton start) end (S.singleton start)
