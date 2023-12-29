import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parseInput :: ReadP (Int, (Int, Int))
parseInput = do
    string "depth: "
    d <- parseNumber
    skipSpaces
    string "target: "
    x <- parseNumber
    char ','
    y <- parseNumber
    skipSpaces
    return (d, (x, y))

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

findErosionLevels :: Int -> (Int, Int) -> M.Map (Int, Int) Int
findErosionLevels depth (tx, ty) = foldl updateMap initMap [ (x, y) | x <- [1..tx], y <- [1..ty] ]
    where initMap = M.fromList $ map (\p -> (p, erosionLevel $ geoIndex p)) edgePoints
          edgePoints = [ (0, y) | y <- [0..ty] ] ++ [ (x, 0) | x <- [1..tx] ]

          updateMap m (x, y) = M.insert (x, y) (erosionLevel geoIdx) m
            where geoIdx = if (x, y) == (tx, ty) then 0 else (m M.! (x-1, y)) * (m M.! (x, y-1))
          erosionLevel = (`mod` 20183) . (+ depth)
          geoIndex (0, 0) = 0
          geoIndex (x, 0) = 16807 * x
          geoIndex (0, y) = 48271 * y
          geoIndex (x, y) = undefined

main :: IO ()
main = do
    (depth, (tx, ty)) <- fmap (runParser parseInput) getContents

    let erosionLevels = findErosionLevels depth (tx, ty)
    print $ sum $ M.elems $ M.map (`mod` 3) erosionLevels
