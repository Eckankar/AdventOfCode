import qualified Data.Array as A
import qualified Data.Map as M
import Data.Maybe (catMaybes)

neighbors (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

main :: IO ()
main = do
    input <- fmap (map (map ((0+) . read . (:[]))) . lines) getContents
    let (height, width) = (length input, length $ head input)
    let inputMap = M.fromList $ A.assocs $ A.listArray ((0, 0), (height-1, width-1)) $ concat input
    print $ M.foldl (+) 0 $ M.mapWithKey (examineNeighbors inputMap) inputMap

    where examineNeighbors m k v =
            if all (>v) $ catMaybes $ map (flip M.lookup m) $ neighbors k
            then v+1
            else 0
