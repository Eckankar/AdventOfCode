import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Text.ParserCombinators.ReadP

data Op = Mul Int Int | Do | Dont
    deriving (Eq, Show)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseDo :: ReadP Op
parseDo = string "do()" >> return Do

parseDont :: ReadP Op
parseDont = string "don't()" >> return Dont

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
    (do o <- (parseMul <|> parseDo <|> parseDont)
        os <- parseInput
        return $ o : os) <++
    (get >> parseInput) <++
    (eof >> return [])

runParser p = fst . head . filter (null . snd) . readP_to_S p

evalOp (Mul n m) = n * m

runProg (rs, _) Do     = (rs, True)
runProg (rs, _) Dont   = (rs, False)
runProg (rs, False) op = (rs, False)
runProg (rs, True)  op = (evalOp op : rs, True)

main :: IO ()
main = do
    ops <- fmap (runParser parseInput) getContents
    print $ sum $ fst $ foldl runProg ([], True) ops
