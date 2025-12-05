import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

import Data.List (sort)
import Debug.Trace (traceShow)

type Range = (Int, Int)

data Marker = RangeOpen Int Int | RangeClose Int Int | Num Int Int
    deriving (Eq, Show)

markerVal (RangeOpen _ i) = (i, 0)
markerVal (Num _ i) = (i, 1)
markerVal (RangeClose _ i) = (i, 2)

instance Ord Marker where
    v1 `compare` v2 = (markerVal v1) `compare` (markerVal v2)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseRange :: ReadP Range
parseRange = do
    lo <- parseInt
    char '-'
    hi <- parseInt
    return (lo, hi)

runParser p = fst . head . filter (null . snd) . readP_to_S p

rangeToMarkers (i, (lo, hi)) = [RangeOpen i lo, RangeClose i hi]

processFreshSpoiled (active, fresh, spoiled) e =
    case e of
        (RangeOpen idx _)  -> (S.insert idx active, fresh, spoiled)
        (RangeClose idx _) -> (S.delete idx active, fresh, spoiled)
        (Num idx v)        -> if S.null active then (active, fresh, v:spoiled) else (active, v:fresh, spoiled)

main :: IO ()
main = do
    (ranges', "":nums') <- fmap (span (/= ""). lines) getContents
    let ranges = map (runParser (parseRange <* eof)) ranges'
    let nums = map (read :: String -> Int) nums'

    let markers' = (concatMap rangeToMarkers $ zip [1..] ranges)
                ++ (map (uncurry Num) $ zip [1..] nums)
    let markers = sort markers'

    let (_, fresh, spoiled) = foldl processFreshSpoiled (S.empty, [], []) markers
    print $ length fresh