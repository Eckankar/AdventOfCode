import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Text.ParserCombinators.ReadP

data Op = Mul Int Int
    deriving (Eq, Show)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseMul :: ReadP Op
parseMul = do
    string "mul("
    n <- parseInt
    char ','
    m <- parseInt
    char ')'
    return $ Mul n m

parseInput :: ReadP [Op]
parseInput =
    (do m <- parseMul
        os <- parseInput
        return $ m : os) <++
    (get >> parseInput) <++
    (eof >> return [])

runParser p = fst . head . filter (null . snd) . readP_to_S p

evalOp (Mul n m) = n * m

main :: IO ()
main = do
    muls <- fmap (runParser parseInput) getContents
    print $ sum $ map evalOp muls
