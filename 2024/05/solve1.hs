import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP

type Order = (Int, Int)
type Update = [Int]

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseOrder :: ReadP Order
parseOrder = do
    x <- parseInt
    char '|'
    y <- parseInt
    return (x, y)

parseUpdate :: ReadP Update
parseUpdate = sepBy parseInt $ char ','

parseInput :: ReadP ([Order], [Update])
parseInput = do
    orders <- sepBy parseOrder $ char '\n'
    string "\n\n"
    updates <- sepBy parseUpdate $ char '\n'
    char '\n'
    return (orders, updates)

runParser p = fst . head . filter (null . snd) . readP_to_S p

makeOrderSet orders = (os, ros)
    where os = S.fromList orders
          ros = S.fromList $ map swap orders

allPairs [] = []
allPairs (x:xs) = [ (x, y) | y <- xs ] ++ allPairs xs

validUpdate orderSet us = all (`S.member` orderSet) ap
    where ap = allPairs us

middleElement ls = ls !! (length ls `div` 2)

main :: IO ()
main = do
    (orders, updates) <- fmap (runParser parseInput) getContents
    let orderSet = S.fromList orders
    let validUpdates = filter (validUpdate orderSet) updates
    print $ sum $ map middleElement validUpdates
