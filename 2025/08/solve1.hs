import qualified Data.Map as M
import qualified Data.Set as S

import Data.List (sort, sortOn)
import Data.List.Split (wordsBy)

sqDist (x, y, z) (x', y', z') = (x-x')*(x-x') + (y-y')*(y-y') + (z-z')*(z-z')

connect m (p1, p2) = foldl (\m' k -> M.insert k newS m') m newS
    where p1s  = m M.! p1
          p2s  = m M.! p2
          newS = S.union p1s p2s

main :: IO ()
main = do
    boxes <- fmap (map ((\[x,y,z] -> (x,y,z)) . map (read :: String -> Int) . wordsBy (== ',')) . lines) getContents
    
    let allPairs = map snd $ sort [ (sqDist b1 b2, (b1, b2))| b1 <- boxes, b2 <- boxes, b1 > b2 ]
    let initMap = M.fromList $ map (\b -> (b, S.singleton b)) boxes

    let numMerges = 1000
    let connMap = foldl connect initMap $ take numMerges allPairs
    print $ foldl1 (*) $ take 3 $ sortOn (\x -> -x) $ map S.size $ S.toList $ S.fromList $ M.elems connMap