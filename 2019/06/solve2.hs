import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isAlphaNum)
import Data.Tuple (swap)
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

bfs m froms to seen = 1 + if S.member to ns' then 0 else bfs m ns' to seen'
    where ns = S.fromList $ concatMap (flip (M.findWithDefault []) m) froms
          ns' = S.difference ns seen
          seen' = S.union seen ns

main :: IO ()
main = do
    input <- fmap (map (runParser (parseOrbit <* eof)) . lines) getContents

    let orbitMap = buildOrbitMap (input ++ map swap input)
    print $ bfs orbitMap (S.singleton "YOU") "SAN" S.empty - 2
