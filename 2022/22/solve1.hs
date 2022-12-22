import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Data.List.Split (wordsBy)
import Text.ParserCombinators.ReadP

data Direction = DLeft | DRight
    deriving (Eq, Show)

data Instruction = Turn Direction | Move Int
    deriving (Eq, Show)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseDirection :: ReadP Direction
parseDirection = (char 'L' >> return DLeft) <|> (char 'R' >> return DRight)

parseInstruction :: ReadP Instruction
parseInstruction = (Move <$> parseInt) <|> (Turn <$> parseDirection)

parseInstructions :: ReadP [Instruction]
parseInstructions = many1 parseInstruction

parseMap :: [String] -> M.Map (Int, Int) Char
parseMap = M.fromList . filter ((/= ' ') . snd) . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [1..] r) . zip [1..]

runParser p = fst . head . filter (null . snd) . readP_to_S p

rotate DLeft  (x, y) = (-y, x)
rotate DRight (x, y) = (y, -x)

neighbors = [ (1, 0), (-1, 0), (0, 1), (0, -1) ]
add (x, y) (x', y') = (x+x', y+y')

findNeighbors :: M.Map (Int, Int) Char -> M.Map ((Int, Int), (Int, Int)) (Int, Int)
findNeighbors m = M.fromList $
    [ ((e,d), e''')
            | e <- ks, d <- neighbors,
              let e' = add e d,
              let e'' = if M.member e' m then e' else findOpposite e d,
              let e''' = if m M.! e'' == '#' then e else e''
    ]
    where ks = M.keys m
          findOpposite (x, y) ( 1, 0) = minimum $ filter ((== y) . snd) ks
          findOpposite (x, y) (-1, 0) = maximum $ filter ((== y) . snd) ks
          findOpposite (x, y) ( 0, 1) = minimum $ filter ((== x) . fst) ks
          findOpposite (x, y) ( 0,-1) = maximum $ filter ((== x) . fst) ks

followPath m ns is p dir = followPath' is p dir
    where followPath' []            p dir = (p, dir)
          followPath' (Move 0 : is) p dir = followPath' is p dir
          followPath' (Move n : is) p dir =
            followPath' (Move (n-1) : is) (ns M.! (p, dir)) dir
          followPath' (Turn d : is) p dir = followPath' is p (rotate d dir)


scoreResult ((r, c), d) = 1000*r + 4*c + scoreDirection d
    where scoreDirection ( 0, 1) = 0
          scoreDirection ( 1, 0) = 1
          scoreDirection ( 0,-1) = 2
          scoreDirection (-1, 0) = 3

main :: IO ()
main = do
    [mapInput, [instructionInput]] <- fmap (wordsBy null . lines) getContents

    let mapMap = parseMap mapInput
    let instructions = runParser (parseInstructions <* eof) instructionInput
    let start = minimum $ M.keys mapMap
    let direction = (0, 1)
    let neighborMap = findNeighbors mapMap

    print $ scoreResult $ followPath mapMap neighborMap instructions start direction
