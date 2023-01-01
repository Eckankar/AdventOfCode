import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Data.List (find)
import Text.ParserCombinators.ReadP

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseIngredient :: ReadP (String, Int)
parseIngredient = do
    count <- parseInt
    char ' '
    name <- many1 $ satisfy isLetter
    return (name, count)

parseReaction :: ReadP (String, (Int, [(String, Int)]))
parseReaction = do
    ingredients <- sepBy parseIngredient (string ", ")
    string " => "
    (name, count) <- parseIngredient
    return (name, (count, ingredients))

runParser p = fst . head . filter (null . snd) . readP_to_S p

mul n = map (\(x, c) -> (x, c*n))

reducedOreCost :: M.Map String (Int, [(String, Int)]) -> M.Map String Int -> Int
reducedOreCost rm cs =
    if M.null nonOreCs then cs M.! "ORE"
    else reducedOreCost rm $ M.foldrWithKey reduceWithRecipe oreCs nonOreCs
        where (oreCs, nonOreCs) = M.partitionWithKey (\k v -> k == "ORE" || v <= 0) cs
              reduceWithRecipe k m cs = foldr addCost cs deps'
                where (rcost, deps) = rm M.! k
                      deps' = (if excess /= 0 then ((k, excess):) else id) $ mul rcount deps
                      rcount = - ((-m) `div` rcost)
                      excess = m - rcost * rcount

              addCost (k, v) m = M.alter addCost' k m
                where addCost' Nothing   = Just v
                      addCost' (Just v') = Just $ v+v'

findMaximalBelow cap rm = binarySearch i j
    where Just ((i, iv), (j, jv)) = find (\((_, n), (_, m)) -> n <= cap && m >= cap) $ zip ocP2 $ tail ocP2
          p2 = 1 : map (*2) p2
          ocP2 = map (\n -> (n, roc n)) p2
          roc n = reducedOreCost rm $ M.singleton "FUEL" n
          binarySearch i j
            | i+1 == j = i
            | otherwise = if midV <= cap then binarySearch mid j
                                         else binarySearch i mid
                where mid = i + (j-i) `div` 2
                      midV = roc mid

main :: IO ()
main = do
    reactions <- fmap (map (runParser (parseReaction <* eof)) . lines) getContents

    let reactionMap = M.fromList reactions
    print $ findMaximalBelow 1000000000000 reactionMap
