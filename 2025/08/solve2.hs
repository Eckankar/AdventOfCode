import qualified Data.Map as M
import qualified Data.Set as S

import Data.List (sort, sortOn)
import Data.List.Split (wordsBy)

type Point = (Int, Int, Int)

fCoord (x, _, _) = x

sqDist (x, y, z) (x', y', z') = (x-x')*(x-x') + (y-y')*(y-y') + (z-z')*(z-z')

connect m (p1, p2) = (newS, foldl (\m' k -> M.insert k newS m') m newS)
    where p1s  = m M.! p1
          p2s  = m M.! p2
          newS = S.union p1s p2s

connectAll :: [Point] -> Int
connectAll ps = connectAll' initMap allPairs
    where allPairs = map snd $ sort [ (sqDist b1 b2, (b1, b2))| b1 <- ps, b2 <- ps, b1 > b2 ]
          initMap = M.fromList $ map (\b -> (b, S.singleton b)) ps
          nps = length ps

          connectAll' :: M.Map Point (S.Set Point) -> [(Point, Point)] -> Int
          connectAll' m ((p1, p2):ps) =
            if p2 `S.member` p1s
            then connectAll' m ps
            else if S.size newS == nps
                 then fCoord p1 * fCoord p2 
                 else connectAll' m' ps
            where p1s        = m M.! p1
                  (newS, m') = connect m (p1, p2)


main :: IO ()
main = do
    boxes <- fmap (map ((\[x,y,z] -> (x,y,z)) . map (read :: String -> Int) . wordsBy (== ',')) . lines) getContents
    print $ connectAll boxes