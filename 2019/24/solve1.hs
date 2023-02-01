import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Integer, Integer)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighbors (x,y) = S.fromList [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

simulate st pAll seen =
    if S.member st' seen'
    then st'
    else simulate st' pAll seen'
    where st' = S.filter isAlive pAll
          seen' = S.insert st seen
          isAlive p = liveNs == 1 || (not alive && liveNs == 2)
            where liveNs = S.size $ S.intersection (neighbors p) st
                  alive = p `S.member` st

bioDiversity ps = sum $ map rating $ S.toList ps
    where rating (x, y) = 2 ^ (5*x+y)

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let allPoints = M.keysSet input
    let initialState = M.keysSet $ M.filter (== '#') input
    print $ bioDiversity $ simulate initialState allPoints S.empty
