import qualified Data.Set as S

import Data.Char (isDigit, isSpace)
import Data.List (find, partition, sort, sortOn)
import Data.List.Extra (minimumOn)
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP

type Point = (Int, Int, Int)
data Scanner = Scanner Int Point [Point]
    deriving (Eq, Show)

data NN = NN Int (S.Set Point)
    deriving (Eq, Show)

data Rotation = RotateX | RotateY | RotateZ
    deriving (Eq, Show)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- many (satisfy isDigit)
    return $ read $ sign : digits

parseHeader = do
    string "--- scanner "
    n <- parseInt
    string " ---\n"
    return n

parsePoint = do
    [x, y, z] <- parseInt `sepBy` char ','
    munch isSpace
    return $ (x, y, z)

parseScanner = do
    n <- parseHeader
    ps <- many parsePoint
    return $ Scanner n (0, 0, 0) ps

parseInputScanners = many parseScanner <* eof

(x, y, z) `pPlus` (x', y', z') = (x + x', y + y', z + z')
(x, y, z) `pMinus` (x', y', z') = (x - x', y - y', z - z')

magnitude (x, y, z) = x*x + y*y + z*z

normalize (x, y, z) = (x', y', z')
    where [x', y', z'] = sort $ map abs [x, y, z]

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

-- Idea: Approximate an overlap by computing the distance to the nearest point for each point.
-- To avoid having to rotate or whatever at this point, normalize each of those differences by
-- taking the absolute differences of each coordinate and sorting the resulting coordinates.
--
-- The idea is basically to try to rank the most promising candidates to explore overlaps on
-- before we start our search proper.
nnDist (Scanner n _ ps) = NN n $ S.fromList $ map findNN ps
    where findNN p = normalize $ minimumOn magnitude $ map (p `pMinus`) $ filter (/= p) ps

pairs []     = []
pairs (x:xs) = map (\y -> (x,y)) xs ++ pairs xs

overlap (NN x xds, NN y yds) = ((x, y), S.size $ S.intersection xds yds)

-- All distinct rotations
allRotations = concatMap rz [ [], [RotateX], [RotateX, RotateX], [RotateX, RotateX, RotateX], [RotateY], [RotateY, RotateY, RotateY] ]
    where rz v = [ v, RotateZ : v, RotateZ : RotateZ : v, RotateZ : RotateZ : RotateZ : v ]

applyRotation ps rs = map (flip (foldl rotate) rs) ps
    where rotate (x, y, z) RotateX = ( x,  z, -y)
          rotate (x, y, z) RotateY = (-z,  y,  x)
          rotate (x, y, z) RotateZ = ( y, -x,  z)

isScanner n (Scanner m _ _) = n == m

tryAlign (Scanner x _ xps) (Scanner y _ yps) = foldl checkRotation Nothing allRotations
    where checkRotation (Just v) _  = Just v
          checkRotation Nothing  rs = uncurry (Scanner y) <$> listToMaybe ryps'
            where ryps = applyRotation yps rs
                  ryps' = [ (yc', ryps') | xs <- xps, ys <- ryps,
                                           let yc' = xs `pMinus` ys,
                                           let ryps' = map (`pPlus` yc') ryps,
                                           S.size (S.intersection (S.fromList xps) (S.fromList ryps')) >= 12
                          ]

alignScanners _       _        []       alignedScanners = alignedScanners
alignScanners aligned overlaps scanners alignedScanners =
    case filter ((`S.member` aligned) . fst) overlaps of
        [] -> error "Argh, pair not available!"
        (x,y) : _ ->
            let Just sx = find (isScanner x) alignedScanners
                Just sy = find (isScanner y) scanners
            in case tryAlign sx sy of
                Nothing -> alignScanners aligned overlaps' scanners alignedScanners
                    where overlaps' = filter (/= (x,y)) overlaps
                Just s -> alignScanners aligned' overlaps' scanners' (s : alignedScanners)
                    where aligned'  = S.insert y aligned
                          overlaps' = filter ((/= y) . snd) overlaps
                          scanners' = filter (not . isScanner y) scanners

scannerCoords (Scanner _ p _) = p

manhattanNorm (x, y, z) = abs x + abs y + abs z

main :: IO ()
main = do
    scanners <- fmap (runParser parseInputScanners) getContents
    let nns = map nnDist scanners
    let overlaps = concatMap (\(x, y) -> [(x, y), (y, x)]) $ map fst $ sortOn ((0-) . snd) $ map overlap $ pairs nns
    let ([sz], scanners') = partition (isScanner 0) scanners
    let overlaps' = filter ((/= 0) . snd) overlaps
    let centres = map scannerCoords $ alignScanners (S.singleton 0) overlaps' scanners' [sz]
    print $ maximum $ map (manhattanNorm . uncurry pMinus) $ pairs centres
