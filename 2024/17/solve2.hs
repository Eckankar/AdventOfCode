import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Bits (xor)
import Data.Char (isDigit)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseRegister :: Char -> ReadP Int
parseRegister c = do
    string $ "Register " ++ [c] ++ ": "
    n <- parseInt
    char '\n'
    return n

parseProgram :: ReadP [Int]
parseProgram = do
    string "Program: "
    ns <- sepBy parseInt (char ',')
    return ns

parseInput :: ReadP ((Int, Int, Int), [Int])
parseInput = do
    a <- parseRegister 'A'
    b <- parseRegister 'B'
    c <- parseRegister 'C'
    char '\n'
    ps <- parseProgram
    skipSpaces
    eof
    return ((a,b,c), ps)

runParser p = fst . head . filter (null . snd) . readP_to_S p

runProgram (a,b,c) ps os ic | ic+2 > length ps = reverse os
runProgram (a,b,c) ps os ic =
    case opCode of
        -- adv
        0 -> runProgram (a `div` (2 ^ combo operand), b, c) ps os (ic+2)
        -- bxl
        1 -> runProgram (a, b `xor` operand, c) ps os (ic+2)
        -- bst
        2 -> runProgram (a, combo operand `mod` 8, c) ps os (ic+2)
        -- jnz
        3 -> runProgram (a,b,c) ps os (if a == 0 then ic+2 else operand)
        -- bxc
        4 -> runProgram (a, b `xor` c, c) ps os (ic+2)
        -- out
        5 -> runProgram (a,b,c) ps ((combo operand `mod` 8) : os) (ic+2)
        -- bdv
        6 -> runProgram (a, a `div` (2 ^ combo operand), c) ps os (ic+2)
        -- bdv
        7 -> runProgram (a, b, a `div` (2 ^ combo operand)) ps os (ic+2)
    where opCode = ps !! ic
          operand = ps !! (ic+1)

          combo n | n >= 0 && n <= 3 = n
          combo 4 = a
          combo 5 = b
          combo 6 = c

printProgram :: [Int] -> [String]
printProgram (opCode : operand : ops) =
    (case opCode of
        -- adv
        0 -> "a := a div 2^" ++ combo operand
        -- bxl
        1 -> "b := b xor " ++ show operand
        -- bst
        2 -> "b := " ++ combo operand ++ " mod 8"
        -- jnz
        3 -> "jnz a " ++ show operand
        -- bxc
        4 -> "b := b xor c"
        -- out
        5 -> "out (" ++ combo operand ++ " mod 8)"
        -- bdv
        6 -> "b := a div 2^" ++ combo operand
        -- cdv
        7 -> "c := a div 2^" ++ combo operand
    ):printProgram ops
    where combo n | n >= 0 && n <= 3 = show n
          combo 4 = "a"
          combo 5 = "b"
          combo 6 = "c"
printProgram _ = []

-- input program:
--
-- b := (a mod 8) xor 1
-- c := a div 2^b
-- a := a div 8
-- b := b xor 4
-- b := b xor c
-- out (b mod 8)
-- jnz a 0

reverseDecode :: [Int] -> [Int] -> S.Set Int -> Int
reverseDecode _ [] ars = S.findMin ars
reverseDecode ps (o:os) ars = reverseDecode ps os ars'
    where ars' = S.fromList [ a' | b <- [0..7], ar' <- S.toList ars, let a' = ar'*8+b, o == head (runProgram (a',0,0) ps [] 0) ]

main :: IO ()
main = do
    ((a,b,c), ps) <- fmap (runParser parseInput) getContents

    putStrLn $ unlines $ printProgram ps

    print $ reverseDecode ps (reverse ps) $ S.singleton 0
