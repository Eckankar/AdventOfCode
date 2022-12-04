import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

data Range = Range Int Int
    deriving (Show, Eq)

parseNumber :: ReadP Int
parseNumber = do
    n <- many (satisfy isDigit)
    return $ read n

parseRange = do
    n <- parseNumber
    char '-'
    m <- parseNumber
    return $ Range n m


parseInputLine = do
    r1 <- parseRange
    char ','
    r2 <- parseRange
    eof
    return (r1, r2)

parseLine = runParser parseInputLine

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

rangeContained r1 r2 = rangeContained' r1 r2 || rangeContained' r2 r1
    where rangeContained' (Range a b) (Range x y) = a <= x && b >= y

main :: IO ()
main = do
    input <- fmap (map parseLine . lines) getContents
    print $ length $ filter (uncurry rangeContained) input
