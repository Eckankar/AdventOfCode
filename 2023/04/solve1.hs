import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

data Card = Card Int [Int] [Int]
    deriving (Eq, Show)

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parseInputLine = do
    string "Card"
    many1 $ char ' '
    id <- parseNumber
    string ":"
    many1 $ char ' '
    winning <- sepBy parseNumber (many1 $ char ' ')
    string " |"
    many1 $ char ' '
    actual <- sepBy parseNumber (many1 $ char ' ')
    eof
    return $ Card id winning actual

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

scoreCard (Card id winning actual) =
    if S.null overlap then 0 else 2 ^ (S.size overlap - 1)
    where overlap = S.intersection (S.fromList winning) $ S.fromList actual

main :: IO ()
main = do
    input <- fmap (map (runParser parseInputLine) . lines) getContents
    print $ sum $ map scoreCard input
