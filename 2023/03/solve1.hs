import qualified Data.Map as M
import qualified Data.Set as S

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

type Coord = (Int, Int)

processInput :: [String] -> (M.Map Coord Char, M.Map Coord Coord, M.Map Coord Int)
processInput input = processInput' input M.empty M.empty M.empty Nothing Nothing (0, 0)
    where processInput' []      symbolMap intPosMap intValueMap _ _ _ = (symbolMap, intPosMap, intValueMap)
          processInput' ([]:is) symbolMap intPosMap intValueMap (Just pos) (Just value) (y, x) =
            processInput' is symbolMap intPosMap (M.insert pos (read $ reverse value) intValueMap) Nothing Nothing (y+1, 0)
          processInput' ([]:is) symbolMap intPosMap intValueMap Nothing Nothing (y, x) =
            processInput' is symbolMap intPosMap intValueMap Nothing Nothing (y+1, 0)
          processInput' ((d:l):is) symbolMap intPosMap intValueMap (Just pos) (Just value) (y, x)
            | isDigit d = processInput' (l:is) symbolMap (M.insert (y, x) pos intPosMap) intValueMap (Just pos) (Just (d:value)) (y, x+1)
            | otherwise = processInput' ((d:l):is) symbolMap intPosMap (M.insert pos (read $ reverse value) intValueMap) Nothing Nothing (y, x)
          processInput' ((d:l):is) symbolMap intPosMap intValueMap Nothing Nothing (y, x)
            | isDigit d = processInput' (l:is) symbolMap (M.insert (y, x) (y, x) intPosMap) intValueMap (Just (y, x)) (Just [d]) (y, x+1)
            | d == '.'  = processInput' (l:is) symbolMap intPosMap intValueMap Nothing Nothing (y, x+1)
            | otherwise = processInput' (l:is) (M.insert (y, x) d symbolMap) intPosMap intValueMap Nothing Nothing (y, x+1)

neighborhood (x,y) = [ (x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1] ]

main :: IO ()
main = do
    input <- fmap lines getContents

    let (symbolMap, intPosMap, intValueMap) = processInput input

    let symbolNeighbors = S.toList $ S.fromList $ concatMap neighborhood $ M.keys symbolMap
    let touchedNumbers = S.toList $ S.fromList $ mapMaybe (flip M.lookup intPosMap) symbolNeighbors
    print $ sum $ map (intValueMap M.!) touchedNumbers

