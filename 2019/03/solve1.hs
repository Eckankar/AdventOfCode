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

expandWire [p] = S.singleton p
expandWire ((x1,y1):(x2,y2):ps) = S.union s $ expandWire $ (x2,y2):ps
    where s = S.fromList [ (x, y) | x <- range x1 x2, y <- range y1 y2 ]

dist (x, y) (x', y') = abs (x-x') + abs (y-y')

main = do
    [w1, w2] <- fmap (map (expandWire . parseWire . wordsBy (==',')) . lines) getContents
    print $ minimum $ S.map (dist (0, 0)) $ S.intersection w1 w2
