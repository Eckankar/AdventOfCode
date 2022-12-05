import Data.Char (isDigit, isSpace)
import Data.List (transpose)
import Data.List.Index (setAt)
import Data.List.Split (wordsBy)
import Text.ParserCombinators.ReadP

data Move = Move Int Int Int
    deriving (Show, Eq)

parseNumber :: ReadP Int
parseNumber = do
    n <- many (satisfy isDigit)
    return $ read n

parseInstruction :: ReadP Move
parseInstruction = do
    string "move "
    n <- parseNumber
    string " from "
    a <- parseNumber
    string " to "
    b <- parseNumber
    eof
    return $ Move n (a-1) (b-1)

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

parseOps = map (runParser parseInstruction)
parseStacks ls = map (dropWhile isSpace) ls''
    where ls' = init ls
          ls'' = transpose $ map (map snd . filter (\(n, _) -> (n - 1) `mod` 4 == 0) . zip [0..]) ls'
          piles = map (const []) $ words $ last ls

parseInput ls = (parseStacks stacks, parseOps ops)
    where [stacks, ops] = wordsBy null ls

executeInstruction stacks (Move n a b) = setAt a aRow' $ setAt b bRow' stacks
    where (moved, aRow') = splitAt n $ stacks !! a
          bRow' = moved ++ stacks !! b

main :: IO ()
main = do
    input <- fmap lines getContents
    let (stacks, ops) = parseInput input
    putStrLn $ map head $ foldl executeInstruction stacks ops
