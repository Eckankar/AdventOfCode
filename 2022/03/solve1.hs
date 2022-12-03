import           Data.Char (ord)
import qualified Data.Set as S
import           Data.Tuple.Extra (both)

type Rucksack = (S.Set Char, S.Set Char)

parseLine :: String -> Rucksack
parseLine s = both S.fromList $ splitAt (length s `div` 2) s

findOverlap :: Rucksack -> Char
findOverlap (c1, c2) = common
    where [common] = S.elems $ S.intersection c1 c2

priority :: Char -> Int
priority x | x >= 'a' && x <= 'z' = ord x - ord 'a' + 1
           | x >= 'A' && x <= 'Z' = ord x - ord 'A' + 27

main :: IO ()
main = do
    input <- fmap (map parseLine . lines) getContents
    print $ sum $ map (priority . findOverlap) input
