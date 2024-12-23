import qualified Data.Map as M
import qualified Data.Set as S

import Data.List.Extra (wordsBy)

toGraph :: [[String]] -> M.Map String (S.Set String)
toGraph is = foldl insertTuple M.empty is'
    where is' = concatMap toTuples is
          toTuples [x, y] = [(x, y), (y, x)]
          insertTuple m (a, b) = M.alter (insertTuple' b) a m
          insertTuple' v (Just s) = Just $ S.insert v s
          insertTuple' v Nothing  = Just $ S.singleton v

findTrips :: M.Map String (S.Set String) -> [(String, String, String)]
findTrips m =
    [ (a, b, c) |
        (a, acons) <- M.toList m',
        (b, bcons) <- M.toList $ M.restrictKeys m' acons,
        let common = S.toList $ S.intersection acons bcons,
        c <- common
    ]
    where m' = trimGraph m
          trimGraph m = M.mapWithKey (\k v -> S.filter (> k) v) m

main :: IO ()
main = do
    input <- fmap (toGraph . map (wordsBy (== '-')) . lines) getContents

    let trips = findTrips input
    print $ length $ filter (\(a,b,c) -> S.member 't' $ S.fromList $ map head [a,b,c]) trips
