import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe (fromJust)

type Point = (Int, Int)

makeMap wh ps = foldl (\m' p -> M.insert p '#' m') m ps
    where m = M.fromList $ [ ((i, j), '.') | i <- [0..wh], j <- [0..wh] ]
                       ++  concat [ [((i, j), '#'), ((j,i), '#')] | i <- [-1, wh+1], j <- [-1..wh+1] ]

add (x, y) (x', y') = (x+x', y+y')

neighbors p = map (add p) [ (1,0), (-1,0), (0,1), (0,-1) ]

bfs :: M.Map Point Char -> M.Map Point Int -> [Point] -> Int -> M.Map Point Int 
bfs m seen [] s = seen
bfs m seen fs s = bfs m seen' fs' (s+1)
    where fs' = filter (\p -> m M.! p == '.') $ S.toList $ S.difference (S.fromList $ concatMap neighbors fs) $ M.keysSet seen
          seen' = M.union seen $ M.fromList $ [ (p, s) | p <- fs' ] 
    

bounds es = ((minX, minY), (maxX, maxY))
    where (minX, maxX) = minMax $ map fst es
          (minY, maxY) = minMax $ map snd es
          minMax (e:es) = foldr (\a (lo, hi) -> (min a lo, max a hi)) (e,e) es

draw :: M.Map Point Char -> [String]
draw m = [ [ m M.! (x, y) | x <- [minX .. maxX] ] | y <- [minY .. maxY] ]
    where ((minX, minY), (maxX, maxY)) = bounds $ M.keys m

showMap m = do
    putStrLn ""
    mapM_ putStrLn $ draw m

main :: IO ()
main = do
    input <- fmap (map ((read :: String -> (Int,Int)) . ('(':) . (++ ")")) . lines) getContents

    --let (mapSize, numBytes) = (6, 12)
    let (mapSize, numBytes) = (70, 1024)
    
    let m = makeMap mapSize $ take numBytes input

    let endIdx = (mapSize, mapSize)
    
    print $ (M.! endIdx) $ bfs m (M.fromList [((0,0), 1)]) [(0,0)] 1 
