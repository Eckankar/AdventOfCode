import qualified Data.Set as S

type Point = (Int, Int)

data Grove = Grove (Point, Point) (S.Set (Point, Char)) Point Point
    deriving (Eq, Show)

startingPoint (Grove _ _ s _) = s
endingPoint (Grove _ _ _ e) = e

moves (x, y) = S.fromList [ (x+1, y), (x, y+1), (x, y), (x-1, y), (x, y-1) ]
add (x, y) (x', y') = (x+x', y+y')

parseInput ls = Grove ((1,1), (h-2,w-2)) bs (0,1) (h-1, w-2)
    where w = length $ head ls
          h = length ls
          ls' = map (tail . init) $ tail $ init ls
          bs = S.fromList $ filter ((/= '.') . snd) $
              concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [1..] r) $ zip [1..] ls'

moveWinds (Grove bounds@((minX, minY), (maxX, maxY)) bs start end) = Grove bounds bs' start end
    where bs' = S.map (uncurry moveWind) bs
          moveWind p d = (keepInBounds $ add p $ dToVec d, d)

          dToVec '>' = (0,1)
          dToVec '<' = (0,-1)
          dToVec '^' = (-1,0)
          dToVec 'v' = (1,0)

          keepInBounds (x, y) =
            (if x < minX then maxX else if x > maxX then minX else x,
             if y < minY then maxY else if y > maxY then minY else y)

findShortestPath _ _ n [] = n
findShortestPath ogs@((Grove bounds@((minX, minY), (maxX, maxY)) bs start end):gs) ps n (t:ts)
    | S.null ps         = error "Ran out of paths..."
    | t `S.member` ps   = findShortestPath ogs (S.singleton t) n ts
    | otherwise         = findShortestPath gs ps'' (n+1) (t:ts)
        where ps' = S.filter inBounds $ S.unions $ S.map moves ps
              ps'' = S.difference ps' $ S.map fst bs

              inBounds p@(x, y) = p == start || p == end ||
                                  x >= minX && x <= maxX && y >= minY && y <= maxY


main :: IO ()
main = do
    input <- fmap (parseInput . lines) getContents

    let groves = tail $ iterate moveWinds input

    let initPos = S.singleton $ startingPoint input
    print $ findShortestPath groves initPos 0 [endingPoint input, startingPoint input, endingPoint input]
