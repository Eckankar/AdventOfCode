import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseLights :: ReadP (S.Set Int)
parseLights = do
    rawLights <- between (char '[') (char ']') $ many1 parseLight
    return $ S.fromList $ map fst $ filter snd $ zip [0..] rawLights
    where parseLight = (char '#' >> return True) <|> (char '.' >> return False)

parseButton :: ReadP (S.Set Int)
parseButton = fmap S.fromList $ between (char '(') (char ')') $ sepBy1 parseInt (char ',')

parseJoltages :: ReadP [Int]
parseJoltages = between (char '{') (char '}') $ sepBy1 parseInt (char ',')

parseInput :: ReadP (S.Set Int, [S.Set Int], [Int])
parseInput = do
    lights <- parseLights
    char ' '
    buttons <- sepBy1 parseButton (char ' ')
    char ' '
    joltages <- parseJoltages

    return (lights, buttons, joltages)

runParser p = fst . head . filter (null . snd) . readP_to_S p

processMachine (targetLights, buttons, joltages) =
    process [(initialLights, 0)] S.empty
    where initialLights = S.empty
          process ((lights, d):qs) seen
            | lights == targetLights = d
            | lights `S.member` seen = process qs seen
            | otherwise = process qs' seen'
                where qs' = qs ++ map ((\v -> (v, d+1)) . pressButton lights) buttons
                      seen' = S.insert lights seen

          pressButton lights button = S.symmetricDifference lights button

main :: IO ()
main = do
    machines <- fmap (map (runParser (parseInput <* eof)) . lines) getContents
    
    print $ sum $ map processMachine machines