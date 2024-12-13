import Algebra.PrincipalIdealDomain (diophantine)
import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Data.Maybe (catMaybes, listToMaybe)
import Text.ParserCombinators.ReadP

import Debug.Trace (traceShow, traceShowId)

type Point = (Int, Int)
data Machine = Machine Point Point Point
    deriving (Eq, Show)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseButton :: ReadP Point
parseButton = do
    string "Button "
    (char 'A' <|> char 'B')
    string ": X+"
    x <- parseInt
    string ", Y+"
    y <- parseInt
    return (x, y)

parsePrize :: ReadP Point
parsePrize = do
    string "Prize: X="
    x <- parseInt
    string ", Y="
    y <- parseInt
    return (x + 10000000000000, y + 10000000000000)

parseMachine :: ReadP Machine
parseMachine = do
    btnA <- parseButton
    char '\n'
    btnB <- parseButton
    char '\n'
    prize <- parsePrize
    return $ Machine btnA btnB prize

parseInput :: ReadP [Machine]
parseInput = do
    machines <- sepBy1 parseMachine $ string "\n\n"
    skipSpaces
    eof
    return machines

runParser p = fst . head . filter (null . snd) . readP_to_S p

-- (Just (-85454545526323,188),Just (-126865671710298,850))
-- (Just (-112537313576298,754),Just (31428571466842,-66))
-- (Nothing,Just (116216216291178,-430))
-- (Just (-34074074137589,92),Just (93943662068396,-667))
-- [Just (0,0),Just (0,0),Just (0,0),Just (0,0)]

mul n (x, y) = (n*x, n*y)
add (x, y) (x', y') = (x+x', y+y')

solve :: Machine -> Maybe (Int, Int)
solve m@(Machine (x, y) (x', y') (tx, ty)) =
    if tx' == tx && ty' == ty
    then Just (sx, sy)
    else Nothing
    where d = det (x, y) (x', y')
          sx = det (tx, ty) (x', y') `div` d
          sy = det (x, y) (tx, ty) `div` d
          det (x, y) (x', y') = x * y' - y * x'
          (tx', ty') = add (mul sx (x,y)) (mul sy (x', y'))

main :: IO ()
main = do
    machines <- fmap (runParser parseInput) getContents
    print $ sum $ map (\(x, y) -> 3*x+y) $ catMaybes $ map solve machines

