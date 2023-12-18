import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Control.Lens.Operators
import Data.Char (isDigit, isHexDigit)
import Numeric.Lens
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

rot (x, y) = (y, -x)

fixInstruction (Instruction _ _ ds) = Instruction d' n' ds
    where d' = fixDir $ last ds
          Just n' = take 5 ds ^? hex

          fixDir '0' = 'R'
          fixDir '1' = 'D'
          fixDir '2' = 'L'
          fixDir '3' = 'U'

add (x, y) (x', y') = (x+x', y+y')
sub (x, y) (x', y') = (x-x', y-y')
mul n (x, y) = (n * x, n * y)
neg (x, y) = (-x, -y)
dir (x, y) = (signum x, signum y)
vmlen (x, y) = abs x + abs y

neighbors (x, y) = S.fromList [ (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]

followInstructions is = reverse $ foldl follow [(0, 0)] is
    where follow (p:ps) (Instruction dc n _) = p':p:ps
            where p' = add p $ mul n $ toDir dc


findArea [] = 0
findArea [(x, y), _, (x', y'), _, (x'', y'')] | (x, y) == (x'', y'') = (abs (x-x') + 1) * (abs (y-y') + 1)
findArea psAll = if psAll' /= psAll then n + findArea psAll' else n + findArea (tail psAll' ++ [head $ tail psAll'])
    where (n, psAll') = simplify psAll [] 0
          simplify (p:p':ps) psr n | p == p' = simplify (p':ps) psr n
          simplify [(0,0),p,(0,0)] [] n = (n + 1 + vmlen p, [])
          simplify (p1:p2:p3:ps) psr n | dir (sub p2 p1) == neg (dir (sub p3 p2)) = simplify (p1:p3:ps) psr $ n + vmlen (sub p3 p2)
          simplify (p1:p2:p3:ps) psr n | dir (sub p2 p1) == dir (sub p3 p2) = simplify (p1:p3:ps) psr n
          simplify ((p1@(x1, y1)):p2:p3:(p4@(x4,y4)):ps) psr n | cwRot p1 p2 p3 p4 && safe p1 p2 p3 p4 =
            simplify (p1:p':p4:ps) psr (n + n')
            where (p', n') = (case (dir $ add (sub p2 p1) (sub p4 p3), dir $ sub p2 p1) of
                                (( 0,  0), _       ) -> ( p1       , (vmlen (sub p2 p1)) * (vmlen (sub p3 p2) + 1) )
                                (( 1,  0), ( 1,  0)) -> ( (x4, y1) , (vmlen (sub p4 p3)) * (vmlen (sub p3 p2) + 1) )
                                (( 1,  0), (-1,  0)) -> ( (x1, y4) , (vmlen (sub p2 p1)) * (vmlen (sub p3 p2) + 1) )
                                (( 0,  1), ( 0,  1)) -> ( (x1, y4) , (vmlen (sub p4 p3)) * (vmlen (sub p3 p2) + 1) )
                                (( 0,  1), ( 0, -1)) -> ( (x4, y1) , (vmlen (sub p2 p1)) * (vmlen (sub p3 p2) + 1) )
                                ((-1,  0), ( 1,  0)) -> ( (x1, y4) , (vmlen (sub p2 p1)) * (vmlen (sub p3 p2) + 1) )
                                ((-1,  0), (-1,  0)) -> ( (x4, y1) , (vmlen (sub p4 p3)) * (vmlen (sub p3 p2) + 1) )
                                (( 0, -1), ( 0,  1)) -> ( (x4, y1) , (vmlen (sub p2 p1)) * (vmlen (sub p3 p2) + 1) )
                                (( 0, -1), ( 0, -1)) -> ( (x1, y4) , (vmlen (sub p4 p3)) * (vmlen (sub p3 p2) + 1) ))
          simplify (p:ps) psr n = simplify ps (p:psr) n
          simplify [] psr n = (n, reverse psr)

          cwRot p1 p2 p3 p4 = d23 == rot d12 && d34 == rot d23
            where d12 = dir (sub p2 p1)
                  d23 = dir (sub p3 p2)
                  d34 = dir (sub p4 p3)

          safe p1@(p1x,p1y) p2@(p2x,p2y) p3 p4@(p4x,p4y) = null unsafe
            where unsafe = filter (not . check) psAll
                  check p | p == p1 || p == p2 || p == p3 || p == p4 = True
                  check (x, y) = checkX || checkY
                    where checkX = (<= 1) $ S.size $ S.difference (S.fromList [signum (p1x - x), signum (p2x - x), signum (p4x - x)]) $ S.singleton 0
                          checkY = (<= 1) $ S.size $ S.difference (S.fromList [signum (p1y - y), signum (p2y - y), signum (p4y - y)]) $ S.singleton 0

main :: IO ()
main = do
    input <- fmap (map (runParser $ parseLine <* eof) . lines) getContents

    let fixedInput = map fixInstruction input
    let corners = followInstructions fixedInput
    print $ findArea corners
