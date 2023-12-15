import Data.Char (ord)
import Data.List.Extra (wordsBy)

hash s = foldl update 0 s
    where update hv c = (hv + ord c) * 17 `mod` 256

main :: IO ()
main = do
    input <- fmap (wordsBy (== ',')) getLine
    print $ sum $ map hash input
