import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP

type Position = (Int, Int)
data Sensor = Sensor Position Position
    deriving (Eq, Show)

data Range = Range Int Int
    deriving (Eq, Show)

data RangeMarker = RangeStart Int | RangeEnd Int
    deriving (Eq, Show)

instance Ord RangeMarker where
    compare m1 m2 = compare (tuplify m1) (tuplify m2)
        where tuplify (RangeStart n) = (n, 1)
              tuplify (RangeEnd n)   = (n, 2)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- many (satisfy isDigit)
    return $ read $ sign : digits

parsePosition :: ReadP Position
parsePosition = do
    string "x="
    x <- parseInt
    string ", y="
    y <- parseInt
    return (x, y)

parseSensor :: ReadP Sensor
parseSensor = do
    string "Sensor at "
    sensorPos <- parsePosition
    string ": closest beacon is at "
    beaconPos <- parsePosition
    return $ Sensor sensorPos beaconPos

runParser :: ReadP a -> String -> a
runParser p = fst . head . filter (null . snd) . readP_to_S p

dist :: Position -> Position -> Int
dist (x, y) (x', y') = abs (x-x') + abs (y-y')

add :: Position -> Position -> Position
add (x, y) (x', y') = (x+x', y+y')

findCoverageRow :: Int -> Sensor -> Maybe Range
findCoverageRow r (Sensor p@(pX, pY) b@(bX, bY)) =
    if dr >= 0 then Just $ Range (pX - dr) (pX + dr) else Nothing
        where dr = dist p b - abs (pY - r)

rangeToMarkers :: Range -> [RangeMarker]
rangeToMarkers (Range x y) = [RangeStart x, RangeEnd y]

mergeRanges :: [Range] -> [Range]
mergeRanges [] = []
mergeRanges rs = reverse rs'
    where (rs', _, _) = foldl genRanges ([], 0, Nothing) rms
          rms = sort $ concatMap rangeToMarkers rs
          genRanges (rs, 0, _)       (RangeStart x) = (rs,   1, Just x)
          genRanges (rs, n, v)       (RangeStart x) = (rs, n+1, v)
          genRanges (rs, 1, Just x') (RangeEnd x)   = (Range x' x : rs, 0, Nothing)
          genRanges (rs, n, v)       (RangeEnd _)   = (rs, n-1, v)

rangeLength (Range a b) = b-a+1

main :: IO ()
main = do
    input <- fmap (map (runParser (parseSensor <* eof)) . lines) getContents
    let row = 2000000
    let beaconsInRow = S.size $ S.fromList $ map (\(Sensor _ (x, _)) -> x)
                                           $ filter (\(Sensor _ (_, y)) -> y == row) input

    let ranges = mergeRanges $ catMaybes $ map (findCoverageRow row) input
    print $ (\x -> x - beaconsInRow) $ sum $ map rangeLength ranges
