import Data.List (sort)
import Data.List.Split (wordsBy)

main :: IO ()
main = do
    input <- fmap lines getContents
    let groups = wordsBy null input
    print $ sum $ take 3 $ reverse $ sort $ map (sum . map (read :: String -> Int)) groups
