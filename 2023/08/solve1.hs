import qualified Data.Map as M

import Text.ParserCombinators.ReadP
import Data.Char (isLetter)

import Debug.Trace (traceShow)

parseMapLine :: ReadP (String, (String, String))
parseMapLine = do
    node <- many1 $ satisfy isLetter
    string " = ("
    left <- many1 $ satisfy isLetter
    string ", "
    right <- many1 $ satisfy isLetter
    char ')'
    eof
    return (node, (left, right))


runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

parseInput (directions:_:ms) = (directions, m)
    where m = M.fromList $ map (runParser parseMapLine) ms

followDirections (d:ds) c t m s
--    | traceShow (d, c, t, s) False = undefined
    | c == t = s
    | otherwise = followDirections ds c' t m (s+1)
        where (l, r) = m M.! c
              c' = if d == 'L' then l else r


main = do
    (directions, desertMap) <- fmap (parseInput . lines) getContents
    print $ followDirections (cycle directions) "AAA" "ZZZ" desertMap 0
