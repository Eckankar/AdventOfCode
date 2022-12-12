import qualified GHC.Arr  as A
import Data.Char (ord)
import qualified Data.Heap as H
import qualified Data.Map as M

type WorldMap = A.Array (Int, Int) Char

neighbors (x, y) = [ (x-1, y), (x+1, y), (x, y-1), (x, y+1) ]

type MinHeap = H.HeapT (H.Prio H.FstMinPolicy (Int, (Int, Int))) (Int, Int)

shortestPath :: WorldMap -> M.Map (Int, Int) Int -> MinHeap -> (Int, Int) -> Int
shortestPath m dm h e
    | p == e          = dm' M.! e
    | p `M.member` dm = shortestPath m dm  h' e
    | otherwise       = shortestPath m dm' h' e
    where Just ((v, p), h'') = H.view h
          ns  = filter (\n -> not (n `M.member` dm) && inRange n && canReach n) $ neighbors p
          h'  = H.filter ((/= p) . snd) $ foldr (\n -> H.insert (v + 1, n)) h'' ns
          dm' = M.insert p v dm
          inRange    = A.inRange (A.bounds m)

          toCmp 'S' = 'a'
          toCmp 'E' = 'z'
          toCmp x   = x
          canReach n = ord x + 1 >= ord y
            where (x, y) = (toCmp $ m A.! p, toCmp $ m A.! n)

arrayify :: [String] -> WorldMap
arrayify lss = A.array (minBound, maxBound) tuples
    where minBound = fst $ head tuples
          maxBound = fst $ last tuples
          tuples = concatMap (uncurry arrayify') $ zip [0..] lss
          arrayify' i ls = map (uncurry (toTuple i)) $ zip [0..] ls
          toTuple i j v = ((i, j), v)

main :: IO ()
main = do
    input <- fmap lines getContents
    let worldMap = arrayify input

    let [(startIdx,_)] = filter ((== 'S') . snd) $ A.assocs worldMap
    let [(endIdx,_)]   = filter ((== 'E') . snd) $ A.assocs worldMap
    print $ shortestPath worldMap M.empty (H.singleton (0, startIdx)) endIdx
