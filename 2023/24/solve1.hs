import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Data.Tuple.Extra (both)
import Text.ParserCombinators.ReadP

type Point = (Int, Int, Int)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- many (satisfy isDigit)
    return $ read $ sign : digits

parsePoint :: ReadP Point
parsePoint = do
    skipSpaces
    x <- parseInt
    skipSpaces
    char ','
    skipSpaces
    y <- parseInt
    skipSpaces
    char ','
    skipSpaces
    z <- parseInt
    skipSpaces
    return (x, y, z)

parseHail :: ReadP (Point, Point)
parseHail = do
    pos <- parsePoint
    char '@'
    vel <- parsePoint
    skipSpaces
    return (pos, vel)

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

toDouble :: (Int, Int, Int) -> (Double, Double, Double)
toDouble (x, y, z) = (fromIntegral x, fromIntegral y, fromIntegral z)
to2D (x, y, _) = (x, y)

pairs (x:xs) = zip (repeat x) xs ++ pairs xs
pairs [] = []

boundedBy lo hi v = v >= lo && v <= hi

-- https://www.topcoder.com/thrive/articles/Geometry%20Concepts%20part%202:%20%20Line%20Intersection%20and%20its%20Applications
checkIntersection lo hi l1@((x1,y1,z1),(dx1,dy1,dz1)) l2@((x2,y2,z2),(dx2,dy2,dz2)) = 
    det /= 0 && boundedBy lo hi intX && boundedBy lo hi intY && m1 >= 0 && m2 >= 0
    where det = - dy1*dx2 + dx1*dy2
          c1 = x1*dy1 - y1*dx1
          c2 = x2*dy2 - y2*dx2
          intX = (- dx2 * c1 + dx1 * c2) / det
          ((_, intY, intZ), m1) = evalAt intX l1
          (_, m2)               = evalAt intX l2
          evalAt v ((x, y, z), (dx, dy, dz)) = ((v, y + m*dy, z + m*dz), m)
            where m = (v-x) / dx

main :: IO ()
main = do
    input <- fmap (map (both toDouble . runParser (parseHail <* eof)) . lines) getContents

    print $ length $ filter (uncurry $ checkIntersection 200000000000000.0 400000000000000.0) $ pairs input
