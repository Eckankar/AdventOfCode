import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Text.ParserCombinators.ReadP

data Regex = RConst Char | RAlt Regex Regex | RConc Regex Regex | REpsilon
    deriving (Eq, Show)

parseRegex'' =
    (RConst <$> choice (map char "NSWE")) <++
    (between (char '(') (char ')') parseRegex)

parseRegex' =
    (foldr1 RConc <$> many1 parseRegex'') <++
    (return REpsilon)

parseRegex =
    (chainr1 parseRegex' (RAlt <$ char '|'))

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

buildMap ps sDoors eDoors REpsilon = (ps, sDoors, eDoors)
buildMap ps sDoors eDoors (RConst 'N') = (ps', sDoors', eDoors)
    where ps' = S.map (\(x,y) -> (x, y-1)) ps
          sDoors' = S.union sDoors $ S.map (\(x,y) -> (x, y-1)) ps
buildMap ps sDoors eDoors (RConst 'S') = (ps', sDoors', eDoors)
    where ps' = S.map (\(x,y) -> (x, y+1)) ps
          sDoors' = S.union sDoors ps
buildMap ps sDoors eDoors (RConst 'W') = (ps', sDoors, eDoors')
    where ps' = S.map (\(x,y) -> (x-1, y)) ps
          eDoors' = S.union eDoors $ S.map (\(x,y) -> (x-1, y)) ps
buildMap ps sDoors eDoors (RConst 'E') = (ps', sDoors, eDoors')
    where ps' = S.map (\(x,y) -> (x+1, y)) ps
          eDoors' = S.union eDoors ps
buildMap ps sDoors eDoors (RAlt rl rr) = (ps', sDoors', eDoors')
    where (psL, sDoorsL, eDoorsL) = buildMap ps S.empty S.empty rl
          (psR, sDoorsR, eDoorsR) = buildMap ps S.empty S.empty rr
          ps' = S.union psL psR
          sDoors' = S.unions [sDoors, sDoorsL, sDoorsR]
          eDoors' = S.unions [eDoors, eDoorsL, eDoorsR]
buildMap ps sDoors eDoors (RConc r r') = (ps'', sDoors''', eDoors''')
    where (ps',  sDoors',  eDoors')  = buildMap ps  S.empty S.empty r
          (ps'', sDoors'', eDoors'') = buildMap ps' S.empty S.empty r'
          sDoors''' = S.unions [sDoors, sDoors', sDoors'']
          eDoors''' = S.unions [eDoors, eDoors', eDoors'']
          

floodFill active seen sDoors eDoors n
    | S.null active = seen
    | otherwise = floodFill active' seen' sDoors eDoors (n+1)
    where active' = S.difference (S.unions $ S.map neighbours active) $ M.keysSet seen
          seen' = M.union seen $ M.fromList $ map (, n) $ S.toList active

          neighbours (x, y) = S.unions [
                if S.member (x,   y  ) eDoors then S.singleton (x+1, y  ) else S.empty,
                if S.member (x-1, y  ) eDoors then S.singleton (x-1, y  ) else S.empty,
                if S.member (x,   y  ) sDoors then S.singleton (x,   y+1) else S.empty,
                if S.member (x,   y-1) sDoors then S.singleton (x,   y-1) else S.empty
            ]

main :: IO ()
main = do
    input <- fmap (runParser (char '^' *> parseRegex <* char '$' <* skipSpaces <* eof)) getContents

    let (_, sDoors, eDoors) = buildMap (S.singleton (0, 0)) S.empty S.empty input

    print $ M.size $ M.filter (>= 1000) $ floodFill (S.singleton (0, 0)) M.empty sDoors eDoors 0 
