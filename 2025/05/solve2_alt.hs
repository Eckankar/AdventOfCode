import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

import Data.List (sortOn)

type Range = (Int, Int)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseRange :: ReadP Range
parseRange = do
    lo <- parseInt
    char '-'
    hi <- parseInt
    return (lo, hi)

runParser p = fst . head . filter (null . snd) . readP_to_S p

mergeRanges ((lo1, hi1) : (lo2, hi2) : rs)
    | hi1 < lo2 = (lo1, hi1) : mergeRanges ((lo2, hi2) : rs)
    | otherwise = mergeRanges ((lo1, max hi1 hi2) : rs)
mergeRanges rs = rs

main :: IO ()
main = do
    ranges <- fmap (map (runParser (parseRange <* eof)) . takeWhile (/= "") . lines) getContents

    let ranges' = mergeRanges $ sortOn fst ranges
    print $ sum $ map (\(lo, hi) -> hi-lo+1) ranges'
