import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..),
                            Alternative((<|>)))
import Data.Char (isDigit)
import Data.Functor (($>))
import Text.ParserCombinators.ReadP

data State = On | Off
    deriving (Show, Eq)
data Instruction = Instruction State (Int, Int) (Int, Int) (Int, Int)
    deriving (Show, Eq)

parseInstruction = (string "on" $> On) <|> (string "off" $> Off)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- many (satisfy isDigit)
    return $ read $ sign : digits

parseRange = do
    start <- parseInt
    string ".."
    end <- parseInt
    return (start, end)

parseLine = do
    onOff <- parseInstruction
    string " x="
    xrange <- parseRange
    string ",y="
    yrange <- parseRange
    string ",z="
    zrange <- parseRange
    return $ Instruction onOff xrange yrange zrange

runParser p = fst . head . filter (null . snd) . readP_to_S p

evaluateInstruction active (Instruction st (xl, xh) (yl, yh) (zl, zh)) =
    case st of
        On  -> S.union active fs
        Off -> S.difference active fs
    where cap n | n > 50    = 51
                | n < -50   = -51
                | otherwise = n
          inRange n = n <= 50 && n >= -50
          fs = S.fromList [ (x, y, z) | x <- [ cap xl .. cap xh ]
                                      , y <- [ cap yl .. cap yh ]
                                      , z <- [ cap zl .. cap zh ]
                                      , all inRange [x,y,z] ]

main :: IO ()
main = do
    instructions <- fmap (map (runParser parseLine) . lines) getContents
    print $ S.size $ foldl evaluateInstruction S.empty instructions
