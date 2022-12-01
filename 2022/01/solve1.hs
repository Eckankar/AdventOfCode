import Data.List.Split (wordsBy)

main :: IO ()
main = do
    input <- fmap lines getContents
    let groups = wordsBy null input
    print $ maximum $ map (sum . map (read :: String -> Int)) groups
