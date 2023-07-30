import Data.Char (isDigit)
import Data.List (sort)
import Text.ParserCombinators.ReadP

data Claim = Claim Int (Int, Int) (Int, Int)
    deriving (Eq, Show)

data SweepMarker = Start Int | End Int
    deriving (Eq, Show)

instance Ord SweepMarker where
    compare m1 m2 = compare (toList m1) (toList m2)
        where toList (Start n) = [n, 1]
              toList (End n)   = [n, 2]

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- many (satisfy isDigit)
    return $ read $ sign : digits

parseLine :: ReadP Claim
parseLine = do
    char '#'
    id <- parseInt
    string " @ "
    left <- parseInt
    char ','
    top <- parseInt
    string ": "
    width <- parseInt
    char 'x'
    height <- parseInt
    return $ Claim id (left, left + width - 1) (top, top + height - 1)

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p


findOverlap claims = sum $ map findOverlapRow [loY .. hiY]
    where vbs = concatMap vertBounds claims
          (loY, hiY) = (minimum vbs, maximum vbs)
          vertBounds (Claim _ _ (t, b)) = [t, b]

          findOverlapRow y = overlap
            where (_, _, overlap) = foldl sweep (Nothing, 0, 0) claims'
                  claims' = sort $ concatMap extractOverlapX claims
                  extractOverlapX (Claim _ (xlo, xhi) (ylo, yhi))
                    | ylo <= y && yhi >= y = [Start xlo, End xhi]
                    | otherwise            = []

                  sweep (Nothing, 1, v) (Start xlo) = (Just xlo, 2, v)
                  sweep (Just xlo, 2, v) (End xhi) = (Nothing, 1, v + xhi - xlo + 1)
                  sweep (p, d, v) (Start _) = (p, d+1, v)
                  sweep (p, d, v) (End _) = (p, d-1, v)

main :: IO ()
main = do
    lines <- fmap (map (runParser (parseLine <* eof)) . lines) getContents
    print $ lines
    print $ findOverlap lines
