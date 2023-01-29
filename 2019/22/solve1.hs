import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

data Instruction = Deal Int | Cut Int | Reverse
    deriving (Show, Eq)

deckSize = 10007

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseInstruction :: ReadP Instruction
parseInstruction =
        Deal <$> (string "deal with increment " >> parseInt)
    <|> Cut <$> (string "cut " >> parseInt)
    <|> (string "deal into new stack" >> return Reverse)

runParser p = fst . head . filter (null . snd) . readP_to_S p

applyInstruction :: (Int -> Int) -> Instruction -> (Int -> Int)
applyInstruction f (Deal n) =
    let m :: M.Map Int Int
        m = M.fromList $ zip ms [0 .. deckSize-1]
        ms = iterate ((`mod` deckSize) . (n +)) 0
    in \i -> f (m M.! i)
applyInstruction f (Cut n)  = \i -> f $ (i + n) `mod` deckSize
applyInstruction f Reverse  = \i -> f $ (deckSize-1) - i

main = do
    input <- fmap (map (runParser (parseInstruction <* eof)) . lines) getContents
    let f = foldl applyInstruction id input
    let [res] = [ x | x <- [0 .. deckSize-1], f x == 2019 ]
    print res
