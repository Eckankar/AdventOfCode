import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

data Round = Round Int Int Int
    deriving (Show, Eq)
data Game = Game Int [Round]
    deriving (Show, Eq)

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parseColor = do
    n <- parseNumber
    char ' '
    color <- (string "red" <|> string "green" <|> string "blue")
    return (n, color)

parseRound = do
    colors <- sepBy parseColor (string ", ")
    let (r, g, b) = foldr fillColor (0, 0, 0) colors
    return $ Round r g b
        where fillColor (n, "red")   (r, g, b) = (n, g, b)
              fillColor (n, "green") (r, g, b) = (r, n, b)
              fillColor (n, "blue")  (r, g, b) = (r, g, n)


parseInputLine = do
    string "Game "
    id <- parseNumber
    string ": "
    rounds <- sepBy parseRound (string "; ")
    eof
    return $ Game id rounds

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p


findGameMax (Game id rounds) = (id, maxes)
    where maxes = foldr findMaxes (0, 0, 0) rounds
          findMaxes (Round r' g' b') (r, g, b) = (max r' r, max g' g, max b' b)

main :: IO ()
main = do
    input <- fmap (map (runParser parseInputLine) . lines) getContents

    print $ sum $ map ((\(_, (r, g, b)) -> r*g*b) . findGameMax) input
