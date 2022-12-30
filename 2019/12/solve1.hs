import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

type Point = (Int, Int, Int)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parsePoint :: ReadP Point
parsePoint = do
    string "<x="
    x <- parseInt
    string ", y="
    y <- parseInt
    string ", z="
    z <- parseInt
    char '>'
    return (x, y, z)

runParser p = fst . head . filter (null . snd) . readP_to_S p

add (x, y, z) (x', y', z') = (x+x', y+y', z+z')

simulate ss = map sim ss
    where sim (p, v) = (p', v')
            where p' = add p v'
                  v' = add v $ foldl add (0, 0, 0) $ map (gravity p) ps
          ps = map fst ss
          gravity (x, y, z) (x', y', z') = (signum (x'-x), signum (y'-y), signum (z'-z))

energy (p, v) = energy' p * energy' v
    where energy' (x, y, z) = abs x + abs y + abs z

main = do
    input <- fmap (map (runParser (parsePoint <* eof)) . lines) getContents

    let initialState = zip input $ repeat (0, 0, 0)
    let states = iterate simulate initialState
    print $ sum $ map energy $ states !! 1000
