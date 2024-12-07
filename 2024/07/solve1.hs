import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Text.ParserCombinators.ReadP

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseLine :: ReadP (Int, [Int])
parseLine = do
    target <- parseInt
    string ": "
    nums <- sepBy parseInt (char ' ')
    return (target, nums)

runParser p = fst . head . filter (null . snd) . readP_to_S p

evaluateOptions :: [S.Set Int] -> S.Set Int
evaluateOptions [ns] = ns
evaluateOptions (ns1 : ns2 : nss) = evaluateOptions $ ns' : nss
    where ns' = S.fromList [ op n1 n2 | n1 <- S.toList ns1, n2 <- S.toList ns2, op <- [(+), (*)] ]

main :: IO ()
main = do
    lines <- fmap (map (runParser parseLine) . lines) getContents

    print $ sum $ map fst $ filter (\(target, ns) -> (target `S.member`) $ evaluateOptions $ map S.singleton ns) lines
