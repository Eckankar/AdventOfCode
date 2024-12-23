import qualified Data.Map as M
import qualified Data.Set as S

import Data.List (intercalate, sort)
import Data.List.Extra (wordsBy)

toGraph :: [[String]] -> M.Map String (S.Set String)
toGraph is = foldl insertTuple M.empty is'
    where is' = concatMap toTuples is
          toTuples [x, y] = [(x, y), (y, x)]
          insertTuple m (a, b) = M.alter (insertTuple' b) a m
          insertTuple' v (Just s) = Just $ S.insert v s
          insertTuple' v Nothing  = Just $ S.singleton v

trimGraph m = M.mapWithKey (\k v -> S.filter (> k) v) m

findTrips :: M.Map String (S.Set String) -> [[(String, S.Set String)]]
findTrips m =
    [ [(c, ccons), (b, bcons), (a, acons)] |
        (a, acons) <- M.toList m,
        (b, bcons) <- M.toList $ M.restrictKeys m acons,
        let common = S.toList $ S.intersection acons bcons,
        c <- common,
        let ccons = m M.! c
    ]

findClique :: M.Map String (S.Set String) -> [[(String, S.Set String)]] -> [[(String, S.Set String)]]
findClique m sets =
    [ (d, dcons):s |
        s <- sets,
        d <- S.toList $ foldl1 S.intersection $ map snd s,
        let dcons = m M.! d
    ]

main :: IO ()
main = do
    input <- fmap (trimGraph . toGraph . map (wordsBy (== '-')) . lines) getContents

    let trips = findTrips input
    let cliques = iterate (findClique input) trips
    putStrLn $ intercalate "," $ sort $ map fst $ head $ head $ dropWhile ((> 1) . length) cliques
