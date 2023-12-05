import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Text.ParserCombinators.ReadP

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parseMap :: ReadP (String, String, Int -> Int)
parseMap = do
    from <- many1 $ satisfy isLetter
    string "-to-"
    to <- many1 $ satisfy isLetter
    string " map:\n"
    mvs <- sepBy (sepBy1 parseNumber (char ' ')) (char '\n')
    let mf = foldl (\f [toS, fromS, l] n -> if n >= fromS && n < fromS+l then n-fromS+toS else f n) id mvs
    return (from, to, mf)

parseInput :: ReadP ([Int], [(String, String, Int -> Int)])
parseInput = do
    string "seeds: "
    seeds <- sepBy parseNumber (char ' ')
    string "\n\n"
    maps <- sepBy parseMap (string "\n\n")
    skipSpaces
    eof
    return (seeds, maps)

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

main :: IO ()
main = do
    (seeds, maps) <- fmap (runParser parseInput) getContents
    let (vs, t) = foldl (\(vs, t) (tf, tt, vmf) -> if tf == t then (map vmf vs, tt) else error "maps not sorted") (seeds, "seed") maps

    if t == "location"
    then print $ minimum $ vs
    else error "location not last map"
