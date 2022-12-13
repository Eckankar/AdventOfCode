import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Data.List.Split (wordsBy)
import Text.ParserCombinators.ReadP

import Debug.Trace (traceShowId)

data NestList = NList [NestList] | NVal Int
    deriving (Show, Eq)

instance Ord (NestList) where
    compare (NVal a)  (NVal b)  = compare a b
    compare (NList a) (NList b) = compare a b
    compare (NVal a)  (NList b) = compare [NVal a] b
    compare (NList a) (NVal b)  = compare a [NVal b]

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parseNestList :: ReadP NestList
parseNestList =
        (NVal <$> parseNumber)
    <|> (NList <$> between (char '[') (char ']') (sepBy parseNestList (char ',')))

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

main :: IO ()
main = do
    input <- fmap (map (map (runParser parseNestList)) . wordsBy null . lines) getContents
    print $ sum $ map fst $ filter (\(_, [a, b]) -> a <= b) $ zip [1..] input
