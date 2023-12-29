import qualified Data.Heap as H
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

data ToolType = Torch | ClimbGear | NoTool
    deriving (Eq, Show, Ord)

type Point = (Int, Int)

type MinHeap = H.MinPrioHeap Int (Point, ToolType)

allTools = [Torch, ClimbGear, NoTool]

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
findErosionLevels depth (tx, ty) = foldl updateMap initMap [ (x, y) | x <- [1..tx'], y <- [1..ty'] ]
    where initMap = M.fromList $ map (\p -> (p, erosionLevel $ geoIndex p)) edgePoints
          edgePoints = [ (0, y) | y <- [0..ty'] ] ++ [ (x, 0) | x <- [1..tx'] ]

          (tx', ty') = (tx + 50, ty + 50)

          updateMap m (x, y) = M.insert (x, y) (erosionLevel geoIdx) m
            where geoIdx = if (x, y) == (tx, ty) then 0 else (m M.! (x-1, y)) * (m M.! (x, y-1))
          erosionLevel = (`mod` 20183) . (+ depth)
          geoIndex (0, 0) = 0
          geoIndex (x, 0) = 16807 * x
          geoIndex (0, y) = 48271 * y
          geoIndex (x, y) = undefined

neighbors (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

shortestPath :: M.Map Point Int -> MinHeap -> S.Set (Point, ToolType) -> (Point, ToolType) -> Int
shortestPath m h seen target
    | (p, t) == target = d
    | (p, t) `S.member` seen = shortestPath m h'' seen target
    | otherwise = shortestPath m h' seen' target
    where Just ((d, (p, t)), h'') = H.view h
          candNs = [ ((p, t'), 7) | t' <- allTools, t' /= t ] ++ [ ((p', t), 1) | p' <- neighbors p ]
          ns = filter (uncurry valid . fst) candNs
          h' = foldr addToHeap h'' ns
          seen' = S.insert (p, t) seen

          valid p' t' = M.member p' m && validTool (m M.! p') t' && not (S.member (p', t') seen)
          validTool 0 NoTool    = False
          validTool 1 Torch     = False
          validTool 2 ClimbGear = False
          validTool _ _         = True

          addToHeap ((p', t'), d') h = H.insert (d+d', (p', t')) h

fromMap m = [ [ toSymb (m M.! (x, y)) | x <- [0..maxX] ] | y <- [0..maxY] ]
    where (maxY, maxX) = maximum $ M.keys m
          toSymb 0 = '.'
          toSymb 1 = '='
          toSymb 2 = '|'

main :: IO ()
main = do
    (depth, (tx, ty)) <- fmap (runParser parseInput) getContents

    let erosionLevels = M.map (`mod` 3) $ findErosionLevels depth (tx, ty)
    --putStrLn $ unlines $ fromMap erosionLevels

    let initialHeap = H.singleton (0, ((0,0), Torch))
    print $ shortestPath erosionLevels initialHeap S.empty ((tx, ty), Torch)
