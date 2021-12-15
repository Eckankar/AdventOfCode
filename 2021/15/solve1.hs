import qualified Data.Array as A
import qualified Data.Heap as H
import qualified Data.Map as M

neighbors (x, y) = [ (x-1, y), (x+1, y), (x, y-1), (x, y+1) ]

type MinHeap = H.HeapT (H.Prio H.FstMinPolicy (Int, (Int, Int))) (Int, Int)

shortestPath :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int -> MinHeap -> (Int, Int) -> Int
shortestPath m dm h e
    | p == e          = dm' M.! e
    | p `M.member` dm = shortestPath m dm  h' e
    | otherwise       = shortestPath m dm' h' e
    where Just ((v, p), h'') = H.view h
          ns  = filter (\n -> n `M.member` m && not (n `M.member` dm)) $ neighbors p
          h'  = H.filter ((/= p) . snd) $ foldr (\n -> H.insert (v + m M.! n, n)) h'' ns
          dm' = M.insert p v dm

main :: IO ()
main = do
    input <- fmap (map (map ((0+) . read . (:[]))) . lines) getContents
    let (height, width) = (length input, length $ head input)
    let inputMap = M.fromList $ A.assocs $ A.listArray ((0, 0), (height-1, width-1)) $ concat input
    print $ shortestPath inputMap M.empty (H.singleton (0, (0, 0))) $ (height-1, width-1)
