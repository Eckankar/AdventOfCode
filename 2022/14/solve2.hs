{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Data.Maybe (catMaybes, isNothing, isJust)
import Text.ParserCombinators.ReadP

type Pair = (Int, Int)

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parsePair :: ReadP Pair
parsePair = do
    x <- parseNumber
    char ','
    y <- parseNumber
    return (x, y)

parseInputLine = sepBy parsePair (string " -> ") <* eof

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

expandLines :: [Pair] -> [Pair]
expandLines xs = concatMap (uncurry expandLine) $ zip xs (tail xs)
    where expandLine (a, b) (a', b') = zip (range a a') $ range b b'
          range a a' | a == a' = repeat a
                     | a > a'  = [ a, a-1..a' ]
                     | a < a'  = [ a..a' ]

updateMinMax :: Pair -> M.Map Int (Int, Int) -> M.Map Int (Int, Int)
updateMinMax (a, b) m = M.alter (updateIfBetter b) a m
    where updateIfBetter x Nothing  = Just (x, x)
          updateIfBetter x (Just (a, b)) = Just (min x a, max x b)

generateMinMaxCol :: [Pair] -> M.Map Int (Int, Int)
generateMinMaxCol = foldr updateMinMax M.empty

dropSand :: Pair -> Int -> Maybe (S.Set Pair, M.Map Int (Int, Int)) -> Maybe (S.Set Pair, M.Map Int (Int, Int))
dropSand _ _ Nothing = Nothing
dropSand p@(dropX, dropY) floorY vs@(Just (points, colMinMaxs))
        | S.member p points = Nothing
        | isNothing (M.lookup dropX colMinMaxs) =
            dropSand p floorY $ Just (S.insert (dropX, floorY) points,
                                      updateMinMax (dropX, floorY) colMinMaxs)
        | dropY > max maxCol floorY = Nothing
        | not $ S.member (dropX, dropY+1) points =
            dropTo (dropX, if dropY < minCol then minCol-1 else dropY+1)
        | not (S.member (dropX-1, dropY+1) points) && dropY+1 < floorY = dropTo (dropX-1, dropY+1)
        | not (S.member (dropX+1, dropY+1) points) && dropY+1 < floorY = dropTo (dropX+1, dropY+1)
        | otherwise = Just (S.insert p points, updateMinMax p colMinMaxs)
    where dropTo (x, y) = dropSand (x, y) floorY vs
          Just (minCol, maxCol) = M.lookup dropX colMinMaxs

main :: IO ()
main = do
    input <- fmap (map (runParser parseInputLine) . lines) getContents
    let initMap' = S.fromList $ concatMap expandLines input
    let floorY = (2+) $ maximum $ map snd $ S.elems initMap'
    let initMap = S.union initMap' $ S.map ((, floorY) . fst) initMap'
    let initMinMaxCol = generateMinMaxCol $ S.elems initMap
    print $ length $ catMaybes $ takeWhile isJust $ tail $ iterate (dropSand (500, 0) floorY) $ Just (initMap, initMinMaxCol)
