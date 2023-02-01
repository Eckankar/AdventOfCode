{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import qualified Data.Set as S

import Data.List (sort)
import Data.Tuple (swap)
import Debug.Trace (traceShow)

type Point = (Integer, Integer)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighbors (p, l) = S.fromList $ concatMap translate offsets
    where offsets = [ (-1, 0), (1, 0), (0, -1), (0, 1) ]
          translate dp =
            case p' of
                (-1, _) -> [((1, 2), l+1)]
                (_, -1) -> [((2, 1), l+1)]
                (5, _)  -> [((3, 2), l+1)]
                (_, 5)  -> [((2, 3), l+1)]
                (2, 2)  -> map (, l-1) $ edges dp
                p       -> [(p, l)]
            where p' = add p dp

          edges (0, 1)  = [ (i, 0) | i <- [0..4] ]
          edges (0, -1) = [ (i, 4) | i <- [0..4] ]
          edges (1, 0)  = [ (0, i) | i <- [0..4] ]
          edges (-1, 0) = [ (4, i) | i <- [0..4] ]

add (x, y) (x', y') = (x+x', y+y')

simulate st = st'
    where st' = S.filter isAlive ns
          ns = S.unions $ S.map neighbors st
          isAlive p = liveNs == 1 || (not alive && liveNs == 2)
            where liveNs = S.size $ S.intersection (neighbors p) st
                  alive = p `S.member` st

iter 0 f v = v
iter n f v = iter (n-1) f v'
    where !v' = traceShow n $ f v

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let initialState = S.map (, 0) $ M.keysSet $ M.filter (== '#') input
    print $ S.size $ iter 200 simulate initialState
