import qualified Data.Set as S

import Data.Char (isDigit)
import Data.List (sort)
import Text.ParserCombinators.ReadP

data Claim = Claim Int (Int, Int) (Int, Int)
    deriving (Eq, Show)

data SweepMarker = Start Int Int | End Int Int
    deriving (Eq, Show)

instance Ord SweepMarker where
    compare m1 m2 = compare (toList m1) (toList m2)
        where toList (Start _ n) = [n, 1]
              toList (End _ n)   = [n, 2]

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


findOverlap claims = S.unions $ map findOverlapRow [loY .. hiY]
    where vbs = concatMap vertBounds claims
          (loY, hiY) = (minimum vbs, maximum vbs)
          vertBounds (Claim _ _ (t, b)) = [t, b]

          findOverlapRow y = overlap
            where (_, _, overlap) = foldl sweep (0, S.empty, S.empty) claims'
                  claims' = sort $ concatMap extractOverlapX claims
                  extractOverlapX (Claim id (xlo, xhi) (ylo, yhi))
                    | ylo <= y && yhi >= y = [Start id xlo, End id xhi]
                    | otherwise            = []

                  sweep (d, active, v) (Start id _) = (d+1, active', v')
                    where active' = S.insert id active
                          v' = if d >= 1 then S.union v active' else v
                  sweep (d, active, v) (End id _) = (d-1, S.delete id active, v)

getId (Claim id _ _) = id

main :: IO ()
main = do
    lines <- fmap (map (runParser (parseLine <* eof)) . lines) getContents
    let overlaps = findOverlap lines
    let ids = S.fromList $ map getId lines

    print $ S.difference ids overlaps
