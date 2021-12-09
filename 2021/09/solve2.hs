import qualified Data.Array as A
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Data.List.Utils (uniq)

neighbors (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

floodfill :: (M.Map (Int, Int) Int) -> Int -> (Int, Int) -> [(Int, Int)]
floodfill m l p | m M.!? p == Nothing = []
floodfill m l p | m M.!? p == Just 9  = []
floodfill m l p | m M.!? p == Just n  =
    if n <= l then [] else p : concatMap (floodfill m n) (neighbors p)
    where Just n = m M.!? p

main :: IO ()
main = do
    input <- fmap (map (map ((0+) . read . (:[]))) . lines) getContents
    let (height, width) = (length input, length $ head input)
    let inputMap = M.fromList $ A.assocs $ A.listArray ((0, 0), (height-1, width-1)) $ concat input
    let lowPoints = M.foldrWithKey (checkLowpoint inputMap) [] inputMap
    print $ foldl (*) 1 $ take 3 $ sortOn (0-) $ map (length . uniq . floodfill inputMap (-1)) lowPoints

    where checkLowpoint m k v ps =
            if all (>v) $ catMaybes $ map (flip M.lookup m) $ neighbors k
            then k : ps
            else ps

