import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Text.ParserCombinators.ReadP

decodeBin :: [Bool] -> Int
decodeBin ds = sum $ map fst $ filter snd $ zip (iterate (*2) 1) ds 

parseHashDot :: ReadP Bool
parseHashDot = (char '#' >> return True) <|> (char '.' >> return False)

parseRule :: ReadP (Int, Bool)
parseRule = do
    input <- many1 parseHashDot
    string " => "
    v <- parseHashDot

    return (decodeBin input, v)

parseInput :: ReadP (S.Set Int, M.Map Int Bool)
parseInput = do
    string "initial state: "
    state <- many1 parseHashDot

    string "\n\n"

    rules <- sepBy (parseRule) (char '\n')
    skipSpaces
    eof

    let stateS = S.fromList $ map fst $ filter snd $ zip [0..] state 
    let ruleMap = M.fromList rules
    return (stateS, ruleMap)

nextState rules state =
    S.fromList [ n | n <- [(lo-2) .. (hi+2)], next n ]
    where lo = minimum state
          hi = maximum state
          next n = M.findWithDefault False (decodeBin [ S.member (n+dn) state | dn <- [-2..2] ]) rules

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

main :: IO ()
main = do
    (initState, ruleMap) <- fmap (runParser parseInput) getContents

    print $ sum $ (!! 20) $ iterate (nextState ruleMap) initState
