import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))

type Point = (Integer, Integer)
data Slope = StraightUp | StraightDown | Slope Bool Rational
    deriving (Eq, Show, Ord)

findAsteroids :: [String] -> [Point]
findAsteroids = map fst . filter ((== '#') . snd) .
                  concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

sub (x, y) (x', y') = (x-x', y-y')

findVisible :: [Point] -> Point -> (Int, Point)
findVisible as p = (S.size $ S.fromList $ mapMaybe (toRatio . flip sub p) as, p)
    where toRatio (x, 0) | x > 0   = Just StraightDown
                         | x < 0   = Just StraightUp
                         | x == 0  = Nothing
          toRatio (x, y)           = Just $ Slope (y > 0) $ x % y


main = do
    asteroids <- fmap (findAsteroids . lines) getContents
    print $ fst $ maximum $ map (findVisible asteroids) asteroids
