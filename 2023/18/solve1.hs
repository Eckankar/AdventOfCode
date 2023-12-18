import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isHexDigit)
import Text.ParserCombinators.ReadP

type Point = (Int, Int)

data Instruction = Instruction Char Int String
    deriving (Show, Eq)

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parseHexColor :: ReadP String
parseHexColor = do
    char '#'
    n <- munch1 isHexDigit
    return $ n

parseLine :: ReadP Instruction
parseLine = do
    d <- choice $ map char "UDLR"
    char ' '
    n <- parseNumber
    char ' '
    c <- between (char '(') (char ')') parseHexColor
    return $ Instruction d n c

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

toDir 'U' = (-1,  0)
toDir 'D' = ( 1,  0)
toDir 'L' = ( 0, -1)
toDir 'R' = ( 0,  1)

add (x, y) (x', y') = (x+x', y+y')

neighbors (x, y) = S.fromList [ (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]

followInstructions is = snd $ foldl follow ((0, 0), S.singleton (0, 0)) is
    where follow (p, seen) (Instruction dc n _) = (last steps, seen')
            where steps = tail $ take (n+1) $ iterate (add $ toDir dc) p
                  seen' = S.union seen $ S.fromList steps

floodFill :: S.Set Point -> S.Set Point -> S.Set Point
floodFill seen ps
    | S.null ps = seen
    | otherwise = floodFill seen' ps'
        where ps'   = flip S.difference seen $ S.unions $ S.map neighbors ps
              seen' = S.union seen ps

followSeam :: S.Set Point -> S.Set Point
followSeam loop = traverse start (startX, startY+1) S.empty
    where start@(startX, startY) = minimum loop
          -- traverse in clockwise direction
          -- start must necessarily be a top left corner
          traverse p@(px,py) p'@(px',py') inside
            | p' == start = inside
            | otherwise = traverse p' pNext inside'
                where [pNext] = S.toList $ S.filter (/= p) $ S.intersection loop $ neighbors p'
                      pNorms = S.map (add ( py'-py, -(px'-px) )) $ S.fromList [p, p']
                      inside' = S.difference (S.union inside pNorms) loop

main :: IO ()
main = do
    input <- fmap (map (runParser $ parseLine <* eof) . lines) getContents

    let path = followInstructions input
    let area = floodFill path $ followSeam path
    print $ S.size area
