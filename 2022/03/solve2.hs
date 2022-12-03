import           Data.Char (ord)
import           Data.List.Split (chunksOf)
import qualified Data.Set as S
import           Data.Tuple.Extra (both)

type Rucksack = [S.Set Char]

parseLines :: String -> [Rucksack]
parseLines = map (map S.fromList) . chunksOf 3 . lines

findOverlap :: Rucksack -> Char
findOverlap rs = common
    where [common] = S.elems $ foldr1 S.intersection rs

priority :: Char -> Int
priority x | x >= 'a' && x <= 'z' = ord x - ord 'a' + 1
           | x >= 'A' && x <= 'Z' = ord x - ord 'A' + 27

main :: IO ()
main = do
    input <- fmap parseLines getContents
    print $ sum $ map (priority . findOverlap) input
