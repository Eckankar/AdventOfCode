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
    return (x, y)

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

solve :: Machine -> Maybe (Int, Int)
solve m@(Machine (x, y) (x', y') (tx, ty)) =
     listToMaybe [ (n, xd)
      | n <- [0..100],
        let (tx', ty') = (tx - n*x, ty - n*y),
        tx' >= 0 && ty' >= 0,
        let (xd, xm) = tx' `divMod` x',
        let (yd, ym) = ty' `divMod` y',
        xm == 0 && ym == 0 && xd == yd
    ]


main :: IO ()
main = do
    machines <- fmap (runParser parseInput) getContents
    print $ sum $ map (\(x, y) -> 3*x+y) $ catMaybes $ map solve machines

