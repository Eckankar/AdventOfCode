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

hasOverlap (xl, xh) (xl', xh') = xl <= xh' && xl' <= xh

splitRange (xl, xh) (xl', xh') | xl < xl'  = (xl, xl'-1) : splitRange (xl', xh) (xl', xh')
                               | xh > xh'  = (xh'+1, xh) : splitRange (xl, xh') (xl', xh')
                               | otherwise = [(xl, xh)]

removeOverlap (Instruction st' x' y' z') (Instruction st x y z)
    | hasOverlap x x' && hasOverlap y y' && hasOverlap z z' =
        [ Instruction st xr yr zr | xr <- splitRange x x'
                                  , yr <- splitRange y y'
                                  , zr <- splitRange z z'
                                  , not $ and [ hasOverlap xr x', hasOverlap yr y', hasOverlap zr z' ]
        ]
    | otherwise = [Instruction st x y z]

applyInstruction is newI =
    case st of
        On  -> newI : is'
        Off -> is'
    where Instruction st _ _ _ = newI
          is' = concatMap (removeOverlap newI) is

volume (Instruction On x y z) = product $ map dist [x, y, z]
    where dist (l, h) = h-l+1
volume _ = 0

main :: IO ()
main = do
    instructions <- fmap (map (runParser parseLine) . lines) getContents
    print $ sum $ map volume $ foldl applyInstruction [] instructions
