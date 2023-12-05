import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Data.List (sort)
import Data.List.Split (chunksOf)
import Text.ParserCombinators.ReadP

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parseMap :: ReadP (String, String, [(Int, Int, Int)])
parseMap = do
    from <- many1 $ satisfy isLetter
    string "-to-"
    to <- many1 $ satisfy isLetter
    string " map:\n"
    mvs <- fmap (sort . map (\[toS, fromS, l] -> (fromS, toS, l))) $ sepBy (sepBy1 parseNumber (char ' ')) (char '\n')
    return (from, to, mvs)

parseInput :: ReadP ([(Int, Int)], [(String, String, [(Int, Int, Int)])])
parseInput = do
    string "seeds: "
    seeds <- fmap (map (\[f, l] -> (f, f+l-1)) . chunksOf 2) $ sepBy parseNumber (char ' ')
    string "\n\n"
    maps <- sepBy parseMap (string "\n\n")
    skipSpaces
    eof
    return (seeds, maps)

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

applyRanges :: [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
applyRanges vmr vsr = applyRanges' vmr (sort vsr)
    where applyRanges' ((mFrom, mTo, mLen):vmr') ((rFrom, rTo):vsr')
            -- starts fully before interval
            | rTo < mFrom =
                (rFrom, rTo) : applyRanges' ((mFrom, mTo, mLen):vmr') vsr'
            -- starts before interval but overlaps
            | rFrom < mFrom && rTo >= mFrom =
                (rFrom, mFrom-1) : applyRanges' ((mFrom, mTo, mLen):vmr') ((mFrom, rTo) : vsr')
            -- contained in interval
            | rFrom >= mFrom && rTo <= mFrom+mLen-1 =
                let startOver ((rFrom', rTo'):vsr'') = rFrom' < rFrom
                    startOver [] = False
                    vmr'' = if startOver vsr' then vmr else (mFrom, mTo, mLen):vmr'
                in (rFrom + (mTo-mFrom), rTo + (mTo-mFrom)) : applyRanges' vmr'' vsr'
            -- starts inside, exceeds right edge of interval
            | rFrom >= mFrom && rFrom <= mFrom+mLen-1 && rTo > mFrom+mLen-1  =
                (rFrom + (mTo-mFrom), mTo+mLen-1) : applyRanges' vmr' ((mFrom+mLen, rTo) : vsr')
            -- starts after interval
            | rFrom > mFrom+mLen-1 =
                applyRanges' vmr' $ (rFrom, rTo) : vsr'

          applyRanges' _ [] = []
          applyRanges' [] vsr' = vsr'

main :: IO ()
main = do
    (seedRanges, maps) <- fmap (runParser parseInput) getContents
    let (vsr, t) = foldl (\(vsr, t) (tf, tt, vmr) -> if tf == t then (applyRanges vmr vsr, tt) else error "maps not sorted") (seedRanges, "seed") maps

    if t == "location"
    then print $ minimum $ map fst vsr
    else error "location not last map"
