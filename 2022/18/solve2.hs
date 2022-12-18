import qualified Data.Set as S
import Data.Function (fix)
import Data.List.Split (splitOn)

type Point = (Int, Int, Int)

parseLine :: [Int] -> Point
parseLine [x, y, z] = (x, y, z)

neighbors (x, y, z) = [
        (x+1, y, z), (x-1, y, z),
        (x, y+1, z), (x, y-1, z),
        (x, y, z+1), (x, y, z-1)
    ]

maxMin [hi, lo] x = [max x hi, min x lo]

growOutside outside cube covered =
        if S.null $ S.difference outside' outside
        then outside
        else growOutside outside' cube covered
    where outside' = S.union outside $ flip S.difference covered $ S.intersection cube ns
          ns = S.fromList $ concatMap neighbors $ S.toList outside

main :: IO ()
main = do
    input <- fmap (map (parseLine . map read . splitOn ",") . lines) getContents
    let pointSet = S.fromList input

    let [maxX, minX] = foldl maxMin [-10000000, 10000000] $ map (\(x,_,_) -> x) input
    let [maxY, minY] = foldl maxMin [-10000000, 10000000] $ map (\(_,y,_) -> y) input
    let [maxZ, minZ] = foldl maxMin [-10000000, 10000000] $ map (\(_,_,z) -> z) input

    let nonCovered = flip S.difference $ S.fromList [ (x, y, z) | x <- [minX .. maxX], y <- [minY .. maxY], z <- [minZ .. maxZ] ]
    let cube = S.fromList [ (x, y, z) | x <- [minX-1 .. maxX+1], y <- [minY-1 .. maxY+1], z <- [minZ-1 .. maxZ+1] ]
    let outside = S.filter (\(x, y, z) -> x < minX || x > maxX || y < minY || y > maxY || z < minZ || z > maxZ) cube

    let outside' = growOutside outside cube pointSet
    print $ sum $ map (S.size . S.intersection outside' . S.fromList . neighbors) input
