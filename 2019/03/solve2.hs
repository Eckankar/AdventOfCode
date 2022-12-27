import qualified Data.Map as M
import qualified Data.Set as S

import Data.List.Split (wordsBy)

add (x, y) (x', y') = (x+x', y+y')
mul n (x, y) = (n*x, n*y)

range n m | n > m  = [n-1, n-2..m]
          | n < m  = [n+1, n+2..m]
          | n == m = [n]

parseWire = scanl follow (0, 0)
    where follow p (d:cs) = add p $ mul c $ dToVec d
            where c = read cs
                  dToVec 'L' = ( 0,-1)
                  dToVec 'R' = ( 0, 1)
                  dToVec 'U' = (-1, 0)
                  dToVec 'D' = ( 1, 0)

expandWire [p] = [p]
expandWire ((x1,y1):(x2,y2):ps) = s ++ expandWire ((x2,y2):ps)
    where s = [ (x, y) | x <- range x1 x2, y <- range y1 y2 ]

dist (x, y) (x', y') = abs (x-x') + abs (y-y')

main = do
    ws <- fmap (map (expandWire . parseWire . wordsBy (==',')) . lines) getContents

    let ms = map (M.fromList . flip zip [1..]) ws
    print $ minimum $ S.map (\i -> sum $ map (M.! i) ms) $ foldl1 S.intersection $ map S.fromList ws
