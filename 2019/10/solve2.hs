import qualified Data.Set as S

import Data.List (sort, sortOn)
import Data.List.Extra (groupOn)
import Data.Ratio ((%))
import Data.Tuple (swap)

type Point = (Integer, Integer)
data Slope = StraightUp | StraightDown | Slope Bool Rational
    deriving (Eq, Show)

instance Ord Slope where
    compare s1 s2 = compare (toList s1) (toList s2)
        where toList StraightUp      = [0]
              toList (Slope True r)  = [1, r]
              toList StraightDown    = [2]
              toList (Slope False r) = [3, r]


findAsteroids :: [String] -> [Point]
findAsteroids = map fst . filter ((== '#') . snd) .
                  concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

add (x, y) (x', y') = (x+x', y+y')
sub (x, y) (x', y') = (x-x', y-y')

toRatio (x, 0) | x > 0   = StraightDown
               | x < 0   = StraightUp
toRatio (x, y)           = Slope (y > 0) $ x % y

groupByRatio as p = map (map (add p) . sortOn size . map snd) $ groupOn fst $ sort $ map (\p -> (toRatio p, p)) as'
    where as' = map (flip sub p) $ filter (/= p) as
          size (x, y) = x*x + y*y

findVisible as p = (length $ groupByRatio as p, p)

eliminationOrder [] = []
eliminationOrder gs = (map head gs) ++ (eliminationOrder $ filter (not . null) $ map tail gs)

main = do
    asteroids <- fmap (findAsteroids . lines) getContents
    let station = snd $ maximum $ map (findVisible asteroids) asteroids
    print $ (\(x, y) -> 100*x+y) $ (!! 199) $ map swap $ eliminationOrder $ groupByRatio asteroids station
