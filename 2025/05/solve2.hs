import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

import Data.List (sort)
import Data.Maybe (fromJust)

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

processRanges (active, fresh, started) e =
    case e of
        (Num idx v)        -> undefined
        (RangeOpen idx v)  -> (S.insert idx active, fresh, started')
            where started' = if S.null active then Just v else started
        (RangeClose idx v) -> (active', fresh', started')
            where active' = S.delete idx active
                  (fresh', started') =
                        if S.null active'
                        then (fresh + v - fromJust started + 1, Nothing)
                        else (fresh, started)                        

main :: IO ()
main = do
    (ranges', "":nums') <- fmap (span (/= ""). lines) getContents
    let ranges = map (runParser (parseRange <* eof)) ranges'
    let nums = map (read :: String -> Int) nums'

    let markers = sort $ concatMap rangeToMarkers $ zip [1..] ranges

    let (_, fresh, _) = foldl processRanges (S.empty, 0, Nothing) markers
    print fresh