import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Text.ParserCombinators.ReadP

data Line = XLine Int (Int, Int) | YLine Int (Int, Int)
    deriving (Show, Eq)

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parseLine :: Char -> Char -> ReadP (Int, (Int, Int))
parseLine x y = do
    char x
    char '='
    xVal <- parseNumber
    string ", "
    char y
    char '='
    yLo <- parseNumber
    string ".."
    yHi <- parseNumber
    return (xVal, (yLo, yHi))

parseInputLine :: ReadP Line
parseInputLine =
    (uncurry XLine <$> parseLine 'y' 'x') <|>
    (uncurry YLine <$> parseLine 'x' 'y')

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

expandLine (XLine y (xLo, xHi)) = S.fromList [ (x, y) | x <- [xLo..xHi] ]
expandLine (YLine x (yLo, yHi)) = S.fromList [ (x, y) | y <- [yLo..yHi] ]

spreadWater q@(x, y) clayTiles waterTiles =
    if null newQs
    then (
        if S.null prevLayerWater
        then ([], waterTiles', clayTiles')
        else spreadWater (head $ S.toList prevLayerWater) clayTiles' waterTiles'
    )
    else (newQs, waterTiles', clayTiles')
    where waterLeft  = takeWhile (\(x', y') -> S.member (x', y'+1) clayTiles && not (S.member (x', y) clayTiles)) [ (x-i, y) | i <- [0..] ]
          waterRight = takeWhile (\(x', y') -> S.member (x', y'+1) clayTiles && not (S.member (x', y) clayTiles)) [ (x+i, y) | i <- [0..] ]
          newWaterTiles = S.unions [S.singleton q, S.fromList waterLeft, S.fromList waterRight]
          waterTiles' = S.union waterTiles newWaterTiles
          clayTiles' = if null newQs then S.union clayTiles newWaterTiles else clayTiles
          (rmX, _) = last waterRight
          (lmX, _) = last waterLeft
          newQs = filter (not . flip S.member clayTiles) [ (lmX-1, y), (rmX+1, y) ]
          prevLayerWater = S.intersection waterTiles $ S.map (\(x', y') -> (x', y'-1)) newWaterTiles

simulateWater [] clayTiles waterTiles _ = (waterTiles, clayTiles)
simulateWater (q@(x,y):qs) clayTiles waterTiles maxY
    | S.member q clayTiles  = simulateWater qs clayTiles waterTiles maxY
    | y > maxY              = simulateWater qs clayTiles waterTiles maxY
    | S.member q waterTiles && not (S.member q clayTiles) = simulateWater qs clayTiles waterTiles maxY
    | not $ S.member (x,y+1) clayTiles = simulateWater ((x,y+1):qs) clayTiles waterTiles' maxY
    | otherwise = simulateWater (qsNew ++ qs) clayTiles' waterTiles'' maxY
    where waterTiles' = S.insert q waterTiles
          (qsNew, waterTiles'', clayTiles') = spreadWater q clayTiles waterTiles'

printMap clayTiles waterTiles stillWaterTiles =
    putStrLn $ unlines [ [ if S.member (x, y) stillWaterTiles then '~' else if S.member (x, y) waterTiles then '|' else if S.member (x, y) clayTiles then '#' else '.' | x <- [minX..maxX] ] | y <- [minY..maxY] ]
    where tiles = S.union clayTiles waterTiles
          minX = minimum $ S.map fst tiles
          maxX = maximum $ S.map fst tiles
          minY = minimum $ S.map snd tiles
          maxY = maximum $ S.map snd tiles
main :: IO ()
main = do
    input <- fmap (map (runParser parseInputLine) . lines) getContents 

    let clayTiles = S.unions $ map expandLine input
    let minY = minimum $ S.map snd clayTiles
    let maxY = maximum $ S.map snd clayTiles

    let (waterTiles, clayTiles') = simulateWater [(500, 0)] clayTiles S.empty maxY
    let waterTiles' = S.filter ((>= minY) . snd) waterTiles 
    let stillWaterTiles = S.difference clayTiles' clayTiles
    printMap clayTiles waterTiles' stillWaterTiles
    print $ S.size waterTiles'
