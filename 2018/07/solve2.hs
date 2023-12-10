import qualified Data.Heap as H
import qualified Data.Set as S

import Data.Char (ord)
import Data.List (sort)

type MinHeap = H.MinPrioHeap Int Char

timeCost c = (ord c - ord 'A' + 1) + 60

resolveOrder :: Int -> Int -> [(Char, Char)] -> S.Set Char -> S.Set Char -> MinHeap -> Int
resolveOrder t free es s sProg pq
  -- make sure we resolve all things on same timestep
  | not (H.null pq) && t' == t =
      resolveOrder t' (free+1) es (S.insert resolved s) (S.delete resolved sProg) pq'
  | S.null sProg && S.null candidates = t
  | not (S.null candidates) && free > 0 =
      resolveOrder t (free-1) es s (S.insert chosen sProg) (H.insert (t + timeCost chosen, chosen) pq)
  | otherwise =
      resolveOrder t' (free+1) es (S.insert resolved s) (S.delete resolved sProg) pq'
  where all = S.difference (S.fromList $ concatMap (\(a, b) -> [a, b]) es) $ S.union s sProg
        candidates = foldr (\(f, t) cs -> if S.member f s then cs else S.delete t cs) all es
        chosen = head $ sort $ S.toList candidates
        Just ((t', resolved), pq') = H.view pq


main :: IO ()
main = do
    input <- fmap (map ((\s -> (s !! 1, s !! 7)) . map head . words) . lines) getContents
    print $ resolveOrder 0 5 input S.empty S.empty H.empty
