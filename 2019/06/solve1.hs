import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isAlphaNum)
import Text.ParserCombinators.ReadP

parseOrbit :: ReadP (String, String)
parseOrbit = do
    a <- many1 $ satisfy isAlphaNum
    char ')'
    b <- many1 $ satisfy isAlphaNum
    return (a, b)

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

buildOrbitMap = foldl (\m (a, b) -> M.alter (app b) a m) M.empty
    where app b Nothing   = Just [b]
          app b (Just bs) = Just (b:bs)

countDepths :: M.Map String [String] -> Int -> String -> Int
countDepths m d p = d + sum (map (countDepths m (d+1)) ps)
    where ps = M.findWithDefault [] p m

main :: IO ()
main = do
    input <- fmap (map (runParser (parseOrbit <* eof)) . lines) getContents

    let orbitMap = buildOrbitMap input
    print $ countDepths orbitMap 0 "COM"
