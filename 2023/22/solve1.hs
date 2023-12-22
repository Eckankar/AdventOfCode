import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.ReadP

type Point = (Int, Int, Int)
data Brick = Brick Point Point
    deriving (Eq, Show)

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parsePoint :: ReadP Point
parsePoint = do
    x <- parseNumber
    char ','
    y <- parseNumber
    char ','
    z <- parseNumber
    return (x, y, z)

parseBrick :: ReadP Brick
parseBrick = do
    start <- parsePoint
    char '~'
    end <- parsePoint
    return $ Brick start end

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

brickParts (Brick (x, y, z) (x', y', z')) =
    [ (x'', y'', z'') | x'' <- [x..x'], y'' <- [y..y'], z'' <- [z..z'] ]

fall1 b@(Brick (x, y, z) (x', y', z'))
    | z == 1 || z' == 1 = b
    | otherwise         = Brick (x, y, z-1) (x', y', z'-1)

brickMap :: [(Int, Brick)] -> M.Map Point Int
brickMap bs = M.fromList $ concatMap (\(n, b) -> zip (brickParts b) $ repeat n) bs

fallOnce bs bm = foldr letFall ([], bm, False) bs
    where letFall (n, b) (bs, bm', fell) = (bs', bm'', fell')
            where b' = fall1 b
                  bs' = (n, if isStopped then b else b') : bs
                  overlap = mapMaybe (flip M.lookup bm') $ brickParts b'
                  isStopped = b == b' || any (/= n) overlap
                  bm'' = if isStopped then bm' else M.union (brickMap [(n,b')]) $ M.filter (/= n) bm'
                  fell' = fell || (not isStopped)
    
fallBricks bs bm = if fell then fallBricks bs' bm' else (bs, bm)
    where (bs', bm', fell) = fallOnce bs bm

canRemove [] _ _ = 0
canRemove ((n,b):bs) bs' bm = (if willFall then 0 else 1) + canRemove bs ((n,b):bs') bm
    where bs'' = bs ++ bs'
          (_, _, willFall) = fallOnce bs'' $ M.filter (/= n) bm

main :: IO ()
main = do
    input <- fmap (map (runParser (parseBrick <* eof)) . lines) getContents

    let input' = zip [1..] input
    let bm = brickMap input'

    let (input'', bm') = fallBricks input' bm
    print $ canRemove input'' [] bm'
