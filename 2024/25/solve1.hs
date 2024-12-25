import Data.List (partition, transpose)
import Data.List.Split (wordsBy)

data Input = Key [Int] | Lock [Int]
    deriving (Eq, Show, Ord)

parse is = if isLock then Lock $ parse' tis else Key $ parse' $ map reverse tis
    where isLock = all (== '#') $ head is
          tis = transpose is
          parse' = map ((\n -> n-1) . length . takeWhile (=='#'))

isLock (Lock _) = True
isLock _ = False

keyFitsLock (Key ks) (Lock ls) = not $ any (> 5) $ zipWith (+) ks ls

main :: IO ()
main = do
    (locks, keys) <- fmap (partition isLock . map parse . wordsBy null . lines) getContents

    print $ length [ (l, k) | l <- locks, k <- keys, keyFitsLock k l ]
