import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Data.List.Split (wordsBy)
import Text.ParserCombinators.ReadP

data Direction = DLeft | DRight
    deriving (Eq, Show)

data Instruction = Turn Direction | Move Int
    deriving (Eq, Show)

data Compass = N | W | S | E
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
parseMap = M.fromList . filter ((/= ' ') . snd) . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

runParser p = fst . head . filter (null . snd) . readP_to_S p

rotate DLeft  (x, y) = (-y, x)
rotate DRight (x, y) = (y, -x)

neighbors = [ (1, 0), (-1, 0), (0, 1), (0, -1) ]
add (x, y) (x', y') = (x+x', y+y')

cubeZone (x, y)   = ( x `div` 50, y `div` 50 )
zoneCoords (x, y) = ( x `mod` 50, y `mod` 50 )

mapToZone (rx, ry) (zx, zy) = (zx * 50 + rx, zy * 50 + ry)

-- Manually map out which sides are adjacent to each other...
cubeNeighbor (0, 1) N = ((3, 0), W)
cubeNeighbor (0, 1) W = ((2, 0), W)
cubeNeighbor (0, 1) S = ((1, 1), N)
cubeNeighbor (0, 1) E = ((0, 2), W)

cubeNeighbor (0, 2) N = ((3, 0), S)
cubeNeighbor (0, 2) W = ((0, 1), E)
cubeNeighbor (0, 2) S = ((1, 1), E)
cubeNeighbor (0, 2) E = ((2, 1), E)

cubeNeighbor (1, 1) N = ((0, 1), S)
cubeNeighbor (1, 1) W = ((2, 0), N)
cubeNeighbor (1, 1) S = ((2, 1), N)
cubeNeighbor (1, 1) E = ((0, 2), S)

cubeNeighbor (2, 1) N = ((1, 1), S)
cubeNeighbor (2, 1) W = ((2, 0), E)
cubeNeighbor (2, 1) S = ((3, 0), E)
cubeNeighbor (2, 1) E = ((0, 2), E)

cubeNeighbor (2, 0) N = ((1, 1), W)
cubeNeighbor (2, 0) W = ((0, 1), W)
cubeNeighbor (2, 0) S = ((3, 0), N)
cubeNeighbor (2, 0) E = ((2, 1), W)

cubeNeighbor (3, 0) N = ((2, 0), S)
cubeNeighbor (3, 0) W = ((0, 1), N)
cubeNeighbor (3, 0) S = ((0, 2), N)
cubeNeighbor (3, 0) E = ((2, 1), S)

dirToCompass (-1, 0) = N
dirToCompass ( 1, 0) = S
dirToCompass ( 0,-1) = W
dirToCompass ( 0, 1) = E

compassToDir N = (-1, 0)
compassToDir S = ( 1, 0)
compassToDir W = ( 0,-1)
compassToDir E = ( 0, 1)

rotateCompass N = W
rotateCompass W = S
rotateCompass S = E
rotateCompass E = N

invCompass = rotateCompass . rotateCompass

findNeighbors :: M.Map (Int, Int) Char -> M.Map ((Int, Int), (Int, Int)) ((Int, Int), (Int, Int))
findNeighbors m = M.fromList $
    [ ((e,d), r)
            | e <- ks, d <- neighbors,
              let e' = add e d,
              let (e'', d') = if M.member e' m then (e', d) else findOpposite e e' d,
              let r = if m M.! e'' == '#' then (e, d) else (e'', d')
    ]
    where ks = M.keys m
          findOpposite e e' d = (mapToZone zp z', compassToDir (invCompass c'))
            where z  = cubeZone e
                  c  = dirToCompass d
                  (z', c') = cubeNeighbor z c
                  zp = rotateToMatch (invCompass c) c' $ zoneCoords e'

          rotateToMatch c c' p | c == c'   = p
                               | otherwise = rotateToMatch (rotateCompass c) c' $ rotateFace p
          rotateFace (x, y) = (49-y, x)


followPath m ns is p dir = followPath' is p dir
    where followPath' []            p dir = (p, dir)
          followPath' (Move 0 : is) p dir = followPath' is p dir
          followPath' (Move n : is) p dir = followPath' (Move (n-1) : is) p' dir'
                where (p', dir') = ns M.! (p, dir)
          followPath' (Turn d : is) p dir = followPath' is p (rotate d dir)


scoreResult ((r, c), d) = 1000*(r+1) + 4*(c+1) + scoreDirection d
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
