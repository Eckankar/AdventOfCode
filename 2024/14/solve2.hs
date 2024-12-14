import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

type Point = (Int, Int)
data Robot = Robot Point Point
    deriving (Eq, Show)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseRobot :: ReadP Robot
parseRobot = do
    string "p="
    px <- parseInt
    char ','
    py <- parseInt
    string " v="
    vx <- parseInt
    char ','
    vy <- parseInt
    return $ Robot (px, py) (vx, vy)

runParser p = fst . head . filter (null . snd) . readP_to_S p

add (x, y) (x', y') = (x+x', y+y')
wrapBounds (bx, by) (x, y) = (x `mod` bx, y `mod` by)

simulate :: Point -> [Robot] -> [Robot]
simulate bounds robots = map simulate' robots
    where simulate' (Robot p v) = Robot p' v
            where p' = wrapBounds bounds $ add p v

divideIntoQuadrants (bx, by) robots = foldl putInQuad ([],[],[],[]) robots
    where putInQuad :: ([Robot],[Robot],[Robot],[Robot]) -> Robot -> ([Robot],[Robot],[Robot],[Robot])
          putInQuad (q1,q2,q3,q4) (Robot (px,py) v) =
                (addIf (px < bxm && py < bym) q1,
                 addIf (px > bxm && py < bym) q2,
                 addIf (px < bxm && py > bym) q3,
                 addIf (px > bxm && py > bym) q4)
                where addIf True  ls = Robot (px,py) v : ls
                      addIf False ls = ls
                      bxm = bx `div` 2
                      bym = by `div` 2

makeMap :: [Robot] -> M.Map Point Char
makeMap rs = foldl addToMap M.empty rs
    where addToMap m (Robot (px, py) v) = M.insert (px, py) '#' m

draw :: Point -> M.Map Point Char -> [String]
draw bounds m = [ [ M.findWithDefault '.' (x,y) m | y <- [minY .. maxY] ] | x <- [minX .. maxX] ]
    where ((minX, minY), (maxX, maxY)) = ((0,0),bounds)

showMap bounds m = do
    putStrLn ""
    mapM_ putStrLn $ draw bounds m

posSet = S.fromList . map (\(Robot p v) -> p)

main :: IO ()
main = do
    robots <- fmap (map (runParser (parseRobot <* eof) ). lines) getContents

    --let bounds = (11, 7)
    let bounds = (101,103)
    let numRobots = length robots
    let simulations = zip [0..] $ iterate (simulate bounds) robots

    let (i, final) = head $ dropWhile (\(i, rs) -> numRobots > S.size (posSet rs)) simulations
    showMap bounds $ makeMap final
    print i
    --let (q1,q2,q3,q4) = divideIntoQuadrants bounds final
    --print $ product $ map length [q1,q2,q3,q4]
