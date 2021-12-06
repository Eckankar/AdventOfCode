import Data.Char (isDigit)
import qualified Data.Map as M
import Text.ParserCombinators.ReadP

data Line = Line (Int, Int) (Int, Int)
    deriving (Show, Eq)

sstring s = skipSpaces >> string s

inputInt = do
    skipSpaces
    digits <- munch1 isDigit
    return $ read digits

inputTuple = do
    skipSpaces
    x <- inputInt
    char ','
    y <- inputInt
    return (x, y)


inputLine = do
    skipSpaces
    startTuple <- inputTuple
    sstring "->"
    endTuple <- inputTuple
    return $ Line startTuple endTuple


inputLines = do
    lines <- many inputLine
    skipSpaces
    eof
    return lines

parseInput :: String -> [Line]
parseInput = fst . head . filter (\(_,"") -> True) . readP_to_S inputLines

isHorzVert (Line (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2

expandLine (Line (x1, y1) (x2, y2)) = zipLong (range x1 x2) (range y1 y2)
    where zipLong [a] [b]       = [(a, b)]
          zipLong [a] (b:bs)    = (a, b) : zipLong [a] bs
          zipLong (a:as) [b]    = (a, b) : zipLong as [b]
          zipLong (a:as) (b:bs) = (a, b) : zipLong as bs

          range a b | a == b = [a]
          range a b | a > b  = a : range (a-1) b
          range a b | a < b  = a : range (a+1) b

countDupes m ls = foldl bumpKey m ls
    where bumpKey m v = M.alter bump v m

          bump Nothing  = Just 1
          bump (Just n) = Just (n+1)

main = do
    lines <- fmap parseInput getContents
    let points = concatMap expandLine lines
    let counts = countDupes M.empty points
    print $ M.size $ M.filter (> 1) counts
