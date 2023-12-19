import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Text.ParserCombinators.ReadP

import Debug.Trace (traceShow, traceShowId)

data Instruction = Cmp Char Char Int String | Jmp String
    deriving (Eq, Show)

type Part = M.Map Char Int

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

checkPart _ "A" p = True
checkPart _ "R" p = False
checkPart is label p = checkPart is label' p
    where check = is M.! label
          label' = applyCheck check

          applyCheck (Jmp s : _) = s
          applyCheck (Cmp attr op n l : cs) =
            if (toOp op) (p M.! attr) n then l else applyCheck cs

          toOp '<' = (<)
          toOp '>' = (>)

main :: IO ()
main = do
    (instructions, parts) <- fmap (runParser (parseInput <* eof)) getContents

    let acceptedParts = filter (checkPart instructions "in") parts
    print $ sum $ map (sum . M.elems) acceptedParts
