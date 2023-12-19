import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Text.ParserCombinators.ReadP

data Instruction = Cmp Char Char Int String | Jmp String
    deriving (Eq, Show)

type Part = M.Map Char Int

data Range = Range Int Int
    deriving (Eq, Show)

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parseInstruction =
    (Jmp <$> munch1 isLetter) <|>
    (do var <- satisfy isLetter
        op <- choice [char '<', char '>']
        n <- parseNumber
        char ':'
        label <- munch1 isLetter
        return $ Cmp var op n label)

parseInstructionLine :: ReadP (String, [Instruction])
parseInstructionLine = do
    name <- munch1 isLetter
    char '{'
    instructions <- sepBy parseInstruction (char ',')
    char '}'
    return (name, instructions)

parseQuality :: ReadP (Char, Int)
parseQuality = do
    name <- satisfy isLetter
    char '='
    n <- parseNumber
    return (name, n)

parsePart :: ReadP Part
parsePart = do
    char '{'
    qualities <- sepBy parseQuality (char ',')
    char '}'
    return $ M.fromList qualities


parseInput :: ReadP (M.Map String [Instruction], [Part])
parseInput = do
    instructions <- sepBy parseInstructionLine (char '\n')
    many1 (char '\n')
    parts <- sepBy parsePart (char '\n')
    skipSpaces
    eof
    return (M.fromList instructions, parts)

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

countPaths :: M.Map String [Instruction] -> String -> String -> M.Map Char Range -> Int
countPaths instructions target current rs
    | current == target = countRanges rs
    | otherwise = sum $ M.elems pathsOn
    where relevantInstructions = M.filter (any leadsHere) instructions
          rangesToHere = M.filter (not . null) $ M.map (filter (all isReachable . M.elems)) $ M.map (toRanges rs) relevantInstructions
          pathsOn = M.mapWithKey (\c' rs' -> sum $ map (countPaths instructions target c') rs') rangesToHere

          leadsHere (Jmp l)       = l == current
          leadsHere (Cmp _ _ _ l) = l == current

          toRanges rs [] = []
          toRanges rs (Jmp l : _) | l == current = [rs]
                                  | otherwise    = []
          toRanges rs (Cmp var op n l : is) =
            (if l == current then (restrictRange rs var r' :) else id) rsRest
            where rsRest = toRanges (restrictRange rs var rFail') is
                  r'      = if op == '<' then Range 1 (n-1) else Range (n+1) 4000
                  rFail'  = if op == '<' then Range n 4000  else Range 1 n

          isReachable (Range lo hi) = lo <= hi

          restrictRange rs var (Range lo' hi') = M.insert var r' rs
            where (Range lo hi) = rs M.! var
                  r' = Range (max lo lo') (min hi hi')

          countRanges rs = product $ map rangeSize $ M.elems rs
          rangeSize (Range lo hi) = hi - lo + 1

main :: IO ()
main = do
    (instructions, parts) <- fmap (runParser (parseInput <* eof)) getContents

    let initialRanges = M.fromList $ map (\e -> (e, Range 1 4000)) "xmas"
    print $ countPaths instructions "in" "A" initialRanges
