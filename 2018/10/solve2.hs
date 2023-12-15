import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

import Debug.Trace (traceShow, traceShowId)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parsePoint :: ReadP (Int, Int)
parsePoint = do
    char '<'
    skipSpaces
    x <- parseInt
    char ','
    skipSpaces
    y <- parseInt
    char '>'
    return (x, y)

parseLine :: ReadP ((Int, Int), (Int, Int))
parseLine = do
    string "position="
    pos <- parsePoint
    string " velocity="
    vel <- parsePoint
    return (pos, vel)

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

add (x, y) (x', y') = (x+x', y+y')
update (p, v) = (add p v, v)

promising vs = (S.size (S.intersection ps ps') * 100) `div` S.size ps > 30
    where ps = S.fromList $ map fst vs
          ps' = S.map (add (0, 1)) ps

showBoard ps = putStrLn $ unlines $ [ [ if S.member (x, y) pss then '#' else '.' | x <- [minX .. maxX] ] | y <- [minY .. maxY] ]
    where (minX, minY) = minimum ps
          (maxX, maxY) = maximum ps
          pss = S.fromList ps

main :: IO ()
main = do
    input <- fmap (map (runParser $ parseLine <* eof) . lines) getContents

    let steps = iterate (map update) input
    let ((n, message):_) = filter (promising . snd) $ zip [0..] steps
    print n
