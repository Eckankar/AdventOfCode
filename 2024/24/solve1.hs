import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Bits ((.&.), (.|.), xor)
import Data.Char (isAlphaNum, isDigit)
import Data.List (sortOn)
import Text.ParserCombinators.ReadP

data Value = Const Int | GAnd String String | GOr String String | GXor String String
    deriving (Eq, Show)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseVarName :: ReadP String
parseVarName = many1 $ satisfy isAlphaNum

parseConst :: ReadP (M.Map String Value)
parseConst = do
    name <- parseVarName 
    string ": "
    value <- parseInt
    return $ M.singleton name $ Const value

parseConsts :: ReadP (M.Map String Value)
parseConsts = fmap M.unions $ sepBy parseConst (char '\n')

parseGateOp :: ReadP (String -> String -> Value)
parseGateOp =
    (string "AND" >> return GAnd) <|>
    (string "OR"  >> return GOr ) <|>
    (string "XOR" >> return GXor)

parseGate :: ReadP (M.Map String Value)
parseGate = do
    v1 <- parseVarName
    char ' '
    op <- parseGateOp
    char ' '
    v2 <- parseVarName
    string " -> "
    target <- parseVarName
    return $ M.singleton target $ op v1 v2

parseGates :: ReadP (M.Map String Value)
parseGates = fmap M.unions $ sepBy parseGate (char '\n')

parseInput :: ReadP (M.Map String Value)
parseInput = do
    consts <- parseConsts
    string "\n\n"
    gates <- parseGates
    skipSpaces
    eof
    return $ M.union consts gates

runParser p = fst . head . filter (null . snd) . readP_to_S p

evaluateGates :: M.Map String Value -> M.Map String Int
evaluateGates m = M.map evaluate m
    where evaluate (Const n) = n
          evaluate (GAnd v1 v2) = evaluate (m M.! v1) .&.   evaluate (m M.! v2)
          evaluate (GOr  v1 v2) = evaluate (m M.! v1) .|.   evaluate (m M.! v2)
          evaluate (GXor v1 v2) = evaluate (m M.! v1) `xor` evaluate (m M.! v2)

main :: IO ()
main = do
    input <- fmap (runParser parseInput) getContents
    let input' = evaluateGates input
    let bits = map snd $ reverse $ sortOn fst $ M.toList $ M.filterWithKey (\k _ -> head k == 'z') input'
    let n = foldl (\a b -> a*2+b) 0 bits
    print n
