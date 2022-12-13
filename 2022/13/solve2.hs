import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Data.List (sort)
import Text.ParserCombinators.ReadP

import Debug.Trace (traceShow)

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

addMarkers xs = marker 2 : marker 6 : xs
    where marker n = NList [NList [NVal n]]

isMarker (NList [NList [NVal n]]) = n == 2 || n == 6
isMarker _ = False

main :: IO ()
main = do
    input <- fmap (map (runParser parseNestList) . filter (not . null) . lines) getContents

    print $ product $ map fst $ filter (isMarker . snd) $ zip [1..] $ sort $ addMarkers input
